#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# <xbar.title>Claude Usage Stats</xbar.title>
# <xbar.version>3.0</xbar.version>
# <xbar.author>Peter Koczan</xbar.author>
# <xbar.author.github>peterkoczan</xbar.author.github>
# <xbar.desc>Real-time Claude Code token usage — session and weekly progress bars matching Claude.ai desktop, burn rate, project breakdown, cost estimates.</xbar.desc>
# <xbar.dependencies>python3,Claude Code (claude.ai/code)</xbar.dependencies>
# <xbar.abouturl>https://github.com/peterkoczan/claude-usage-bar</xbar.abouturl>
# <swiftbar.refreshOnOpen>true</swiftbar.refreshOnOpen>
# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.environment>[ANTHROPIC_API_KEY=]</swiftbar.environment>

"""
Claude Usage Stats v3 — SwiftBar plugin
────────────────────────────────────────
Shows Claude Code token usage with progress bars matching the
"Settings → Usage" view in Claude.ai desktop.

LIMITS CONFIG
  Edit or create  ~/.claude_usage.json  to set your plan limits:

    {
      "plan":                 "pro",
      "weekly_limit":          1000000,
      "session_window_hours":  5,
      "session_limit":         200000,
      "pricing_input":         3.00,
      "pricing_output":        15.00
    }

  Plan defaults (auto-applied if you set "plan" without explicit limits):
    free   weekly=100K    session=20K
    pro    weekly=1M      session=200K
    team   weekly=2M      session=400K
    max    weekly=5M      session=800K

INSTALL
  1. brew install --cask swiftbar
  2. Open SwiftBar, choose plugin folder (e.g. ~/SwiftBarPlugins)
  3. cp claude_usage.1m.py ~/SwiftBarPlugins/
  4. chmod +x ~/SwiftBarPlugins/claude_usage.1m.py
  5. Click menu bar icon -> Refresh All

OPTIONAL - Anthropic API key for org-level usage
  Right-click plugin icon -> Plugin settings
  Add: ANTHROPIC_API_KEY = sk-ant-...
"""

import json
import os
import subprocess
import time
from datetime import datetime, date, timedelta
from pathlib import Path
from collections import defaultdict

# ── Paths & env ───────────────────────────────────────────────────────────────

CLAUDE_DIR   = Path.home() / ".claude" / "projects"
CONFIG_FILE  = Path.home() / ".claude_usage.json"
API_KEY      = os.environ.get("ANTHROPIC_API_KEY", "")
RL_CACHE     = Path("/tmp/claude_usage_rl.json")
RL_CACHE_TTL = 60   # seconds between live API calls; display still refreshes every 15s

# ── Plan defaults ─────────────────────────────────────────────────────────────

PLAN_DEFAULTS = {
    "free": {"weekly_limit":   100_000, "session_limit":  20_000},
    "pro":  {"weekly_limit": 1_000_000, "session_limit": 200_000},
    "team": {"weekly_limit": 2_000_000, "session_limit": 400_000},
    "max":  {"weekly_limit": 5_000_000, "session_limit": 800_000},
}

def load_config() -> dict:
    cfg = {
        "plan":                 "pro",
        "weekly_limit":         1_000_000,
        "session_window_hours": 5,
        "session_limit":        200_000,
        "pricing_input":        3.00,
        "pricing_output":       15.00,
    }
    if CONFIG_FILE.exists():
        try:
            user = json.loads(CONFIG_FILE.read_text())
            plan = user.get("plan", "pro").lower()
            if plan in PLAN_DEFAULTS and "weekly_limit" not in user:
                cfg.update(PLAN_DEFAULTS[plan])
            cfg.update(user)
        except Exception:
            pass
    else:
        try:
            CONFIG_FILE.write_text(json.dumps(cfg, indent=2) + "\n")
        except Exception:
            pass
    return cfg

# ── Formatting ────────────────────────────────────────────────────────────────

def fmt_tokens(n: int) -> str:
    if n >= 1_000_000: return f"{n/1_000_000:.1f}M"
    if n >= 1_000:     return f"{n/1_000:.1f}K"
    return str(n) if n else "0"

def fmt_cost(inp: int, out: int, cfg: dict) -> str:
    c = inp / 1_000_000 * cfg["pricing_input"] + out / 1_000_000 * cfg["pricing_output"]
    if c < 0.0001: return "< $0.0001"
    if c < 0.01:   return f"~${c:.4f}"
    if c < 1.0:    return f"~${c:.3f}"
    return f"~${c:.2f}"

def fmt_duration(hours: float) -> str:
    """Format a duration in hours as '2h 30m', '1d 4h', etc."""
    if hours < 0:    return "—"
    if hours < 1/60: return "< 1m"
    if hours < 1:    return f"{int(hours * 60)}m"
    h = int(hours)
    m = int((hours - h) * 60)
    if h >= 48:
        d = h // 24
        return f"{d}d"
    if h >= 24:
        d = h // 24
        rh = h % 24
        return f"{d}d {rh}h" if rh else f"{d}d"
    return f"{h}h {m}m" if m else f"{h}h"

def parse_ts(raw):
    try:
        if isinstance(raw, (int, float)): return datetime.fromtimestamp(raw)
        return datetime.fromisoformat(str(raw)[:19])
    except Exception:
        return datetime.now()

# ── Progress bar ──────────────────────────────────────────────────────────────

BAR_WIDTH = 20

def bar_color(pct: float) -> str:
    if pct >= 0.90: return "#EF4444"
    if pct >= 0.70: return "#F59E0B"
    return "#A78BFA"

def make_bar(used: int, limit: int):
    pct    = min(used / limit, 1.0) if limit > 0 else 0.0
    filled = round(pct * BAR_WIDTH)
    bar    = "\u2588" * filled + "\u2591" * (BAR_WIDTH - filled)
    return bar, pct, bar_color(pct)

# ── Parse local Claude Code sessions ──────────────────────────────────────────

BURN_WINDOW_HOURS = 3  # look back this far to compute burn rate

def parse_local(session_window_hours: float = 5.0):
    """
    Walk ~/.claude/projects/**/*.jsonl.
    Returns:
      by_date       – {date: {in, out, cache_r, cache_w}}
      by_project    – {Path: {in, out}}  (all-time per project dir)
      n_files       – total .jsonl files found
      sess_in/out   – tokens in the rolling session window
      recent_in/out – tokens in the last BURN_WINDOW_HOURS (for burn rate)
    """
    by_date    = defaultdict(lambda: {"in": 0, "out": 0, "cache_r": 0, "cache_w": 0})
    by_project = defaultdict(lambda: {"in": 0, "out": 0})
    n_files    = 0
    sess_in = sess_out = 0
    recent_in = recent_out = 0

    now            = datetime.now()
    cutoff_sess    = now - timedelta(hours=session_window_hours)
    cutoff_recent  = now - timedelta(hours=BURN_WINDOW_HOURS)

    if not CLAUDE_DIR.exists():
        return by_date, by_project, n_files, sess_in, sess_out, recent_in, recent_out

    for jsonl in CLAUDE_DIR.rglob("*.jsonl"):
        # Always attribute tokens to the top-level project dir so that
        # nested subagent JSONL files roll up into their parent project.
        try:
            top_proj = CLAUDE_DIR / jsonl.relative_to(CLAUDE_DIR).parts[0]
        except (ValueError, IndexError):
            top_proj = jsonl.parent
        proj_dir = top_proj
        n_files += 1
        try:
            lines = jsonl.read_text(errors="ignore").splitlines()
        except OSError:
            continue

        for raw_line in lines:
            raw_line = raw_line.strip()
            if not raw_line:
                continue
            try:
                msg = json.loads(raw_line)
            except json.JSONDecodeError:
                continue

            ts_raw = msg.get("timestamp") or msg.get("ts") or msg.get("created_at")
            ts     = parse_ts(ts_raw) if ts_raw else now
            d      = ts.date()

            usage = msg.get("usage") or {}
            if not usage:
                inner = msg.get("message")
                if isinstance(inner, dict):
                    usage = inner.get("usage") or {}

            inp = int(usage.get("input_tokens")                or 0)
            out = int(usage.get("output_tokens")               or 0)
            cr  = int(usage.get("cache_read_input_tokens")     or 0)
            cw  = int(usage.get("cache_creation_input_tokens") or 0)

            if inp or out:
                by_date[d]["in"]      += inp
                by_date[d]["out"]     += out
                by_date[d]["cache_r"] += cr
                by_date[d]["cache_w"] += cw
                by_project[proj_dir]["in"]  += inp
                by_project[proj_dir]["out"] += out
                if ts >= cutoff_sess:
                    sess_in  += inp
                    sess_out += out
                if ts >= cutoff_recent:
                    recent_in  += inp
                    recent_out += out

    return by_date, by_project, n_files, sess_in, sess_out, recent_in, recent_out

# ── Live rate limits via API response headers ─────────────────────────────────

def get_oauth_token() -> str:
    """Read Claude Code OAuth token from macOS Keychain."""
    try:
        r = subprocess.run(
            ["security", "find-generic-password", "-s", "Claude Code-credentials", "-w"],
            capture_output=True, text=True, timeout=5
        )
        if r.returncode != 0:
            return ""
        creds = json.loads(r.stdout.strip())
        return creds.get("claudeAiOauth", {}).get("accessToken", "")
    except Exception:
        return ""

def fetch_live_rate_limits(token: str):
    """
    Return authoritative rate limit utilization from Anthropic response headers.

    Results are cached to RL_CACHE for RL_CACHE_TTL seconds so the display
    can refresh every 15 s without hammering the API every 15 s.

    Why a minimal inference call: Anthropic embeds exact utilization % in every
    API response. Cost at max_tokens=1 with a warm cache ≈ 1 fresh input token
    per cache-miss (every 60 s), not per display refresh.
    """
    if not token:
        return None

    # ── Cache hit? ────────────────────────────────────────────────────────────
    now = time.time()
    try:
        if RL_CACHE.exists():
            cached = json.loads(RL_CACHE.read_text())
            if now - cached.get("ts", 0) < RL_CACHE_TTL:
                return cached.get("data")
    except Exception:
        pass

    # ── Cache miss — call the API ─────────────────────────────────────────────
    try:
        import urllib.request
        body = json.dumps({
            "model":      "claude-haiku-4-5-20251001",
            "max_tokens": 1,
            "messages":   [{"role": "user", "content": "."}],
        }).encode()
        req = urllib.request.Request(
            "https://api.anthropic.com/v1/messages",
            data=body,
            headers={
                "x-api-key":         token,
                "anthropic-version": "2023-06-01",
                "content-type":      "application/json",
            },
        )
        with urllib.request.urlopen(req, timeout=10) as r:
            h = dict(r.headers)
            data = {
                "5h_util":  float(h.get("anthropic-ratelimit-unified-5h-utilization",  0)),
                "7d_util":  float(h.get("anthropic-ratelimit-unified-7d-utilization",  0)),
                "5h_reset": int(h.get("anthropic-ratelimit-unified-5h-reset", 0)),
                "7d_reset": int(h.get("anthropic-ratelimit-unified-7d-reset", 0)),
                "status":   h.get("anthropic-ratelimit-unified-status", ""),
            }
        try:
            RL_CACHE.write_text(json.dumps({"ts": now, "data": data}))
        except Exception:
            pass
        return data
    except Exception:
        return None

# ── Optional: Anthropic API usage ─────────────────────────────────────────────

def fetch_api_usage():
    if not API_KEY:
        return None
    try:
        import urllib.request
        start = (date.today() - timedelta(days=29)).isoformat()
        url   = f"https://api.anthropic.com/v1/usage?start_date={start}&granularity=day"
        req   = urllib.request.Request(url, headers={
            "x-api-key":         API_KEY,
            "anthropic-version": "2023-06-01",
        })
        with urllib.request.urlopen(req, timeout=5) as r:
            return json.loads(r.read())
    except Exception as e:
        return {"_error": str(e)}

# ── Window aggregation ────────────────────────────────────────────────────────

def window(by_date, start: date, end: date = None):
    end = end or date.today()
    inp = out = cr = cw = 0
    d = start
    while d <= end:
        s    = by_date.get(d, {})
        inp += s.get("in", 0)
        out += s.get("out", 0)
        cr  += s.get("cache_r", 0)
        cw  += s.get("cache_w", 0)
        d   += timedelta(days=1)
    return inp, out, cr, cw

# ── Project name decoding ─────────────────────────────────────────────────────

def project_name(proj_path: Path) -> str:
    """
    Claude encodes project paths by replacing '/' with '-'.
    e.g. /Users/alice/Developer/my-app → -Users-alice-Developer-my-app
    We strip the home directory prefix and common parent dirs to get
    a readable project name like 'my-app' or 'Developer/my-app'.
    """
    encoded = proj_path.name
    home    = str(Path.home()).lstrip("/")          # "Users/alice"
    prefix  = "-" + home.replace("/", "-") + "-"   # "-Users-alice-"

    home_encoded = "-" + home.replace("/", "-")     # "-Users-alice" (no trailing dash)
    if encoded == home_encoded:
        return "~"                                  # sessions run from home directory

    if encoded.startswith(prefix):
        remainder = encoded[len(prefix):]           # "Developer-my-app"
        # Strip one common parent dir (Developer, Projects, Code, src, workspace)
        for skip in ("Developer-", "Projects-", "Code-", "src-", "workspace-", "repos-"):
            if remainder.startswith(skip):
                remainder = remainder[len(skip):]
                break
        return remainder or encoded
    # Fallback: strip leading dash, return last two dash-segments for context
    parts = [p for p in encoded.lstrip("-").split("-") if p]
    return "-".join(parts[-2:]) if len(parts) >= 2 else (parts[-1] if parts else encoded)

# ── SwiftBar output helpers ───────────────────────────────────────────────────

def ln(text: str, **attrs):
    if attrs:
        print(f"{text} | " + " ".join(f"{k}={v}" for k, v in attrs.items()))
    else:
        print(text)

def progress_section(label: str, used: int, limit: int):
    bar, pct, color = make_bar(used, limit)
    remaining       = max(limit - used, 0)
    ln(f"{bar}  {pct*100:.0f}%", color=color, font="Menlo", size=11)
    ln(f"{label}   {fmt_tokens(used)} used  ·  {fmt_tokens(remaining)} left  /  {fmt_tokens(limit)}",
       color="#94A3B8", size=11)

# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    cfg = load_config()

    today     = date.today()
    yesterday = today - timedelta(days=1)
    week_ago  = today - timedelta(days=6)
    month_ago = today - timedelta(days=29)

    by_date, by_project, n_files, sess_in, sess_out, recent_in, recent_out = parse_local(
        session_window_hours=cfg["session_window_hours"]
    )

    t_in, t_out, t_cr, t_cw = window(by_date, today)
    y_in, y_out, *_          = window(by_date, yesterday, yesterday)
    w_in, w_out, *_          = window(by_date, week_ago)
    m_in, m_out, *_          = window(by_date, month_ago)

    total_week   = w_in + w_out
    total_month  = m_in + m_out

    weekly_limit  = int(cfg["weekly_limit"])
    session_limit = int(cfg["session_limit"])

    # ── Try to get authoritative rate limits from the API ─────────────────────
    # Anthropic returns exact utilization % in every API response header.
    # This matches the Claude.ai desktop numbers precisely.
    oauth_token = get_oauth_token()
    live        = fetch_live_rate_limits(oauth_token)

    if live:
        w_pct             = live["7d_util"]
        s_pct             = live["5h_util"]
        week_limit_used   = int(w_pct * weekly_limit)
        session_limit_used= int(s_pct * session_limit)
        week_reset_in_h   = (live["7d_reset"] - time.time()) / 3600 if live["7d_reset"] else -1
        sess_reset_in_h   = (live["5h_reset"] - time.time()) / 3600 if live["5h_reset"] else -1
        live_source       = True
    else:
        # Fallback: use input_tokens from local JSONL
        week_limit_used    = w_in
        session_limit_used = sess_in
        w_pct              = min(week_limit_used  / weekly_limit,  1.0) if weekly_limit  else 0.0
        s_pct              = min(session_limit_used / session_limit, 1.0) if session_limit else 0.0
        week_reset_in_h    = -1
        sess_reset_in_h    = -1
        live_source        = False

    # ── Burn rate: input_tokens/hour over last BURN_WINDOW_HOURS ─────────────
    burn_rate = recent_in / BURN_WINDOW_HOURS

    now = datetime.now()

    # ── Menu bar label ─────────────────────────────────────────────────────────
    dominant_pct = max(w_pct, s_pct)
    col          = bar_color(dominant_pct)

    if t_out == 0:
        print("⚡ idle | color=#6B7280")
    else:
        pct_display = f"{dominant_pct*100:.0f}%"
        if burn_rate >= 500:
            print(f"⚡ {fmt_tokens(t_out)} · {pct_display} · {fmt_tokens(int(burn_rate))}/h | color={col}")
        else:
            print(f"⚡ {fmt_tokens(t_out)} · {pct_display} | color={col}")
    print("---")

    # ── Usage limits ──────────────────────────────────────────────────────────
    ln("USAGE LIMITS", color="#F8FAFC", font="Helvetica-Bold", size=10)

    progress_section("weekly  (7 days)", week_limit_used, weekly_limit)
    if week_reset_in_h > 0:
        ln(f"↻  resets in {fmt_duration(week_reset_in_h)}",
           color="#475569", size=10)

    print(" ")

    progress_section("session (5h rolling)", session_limit_used, session_limit)
    if sess_reset_in_h > 0:
        ln(f"↻  resets in {fmt_duration(sess_reset_in_h)}",
           color="#475569", size=10)
    if burn_rate >= 100:
        ln(f"⚡  {fmt_tokens(int(burn_rate))}/h burn rate  (last {BURN_WINDOW_HOURS}h)",
           color="#94A3B8", size=10)

    if not live_source:
        ln("⚠  offline — showing Claude Code CLI estimate only", color="#F59E0B", size=10)
    ln("Edit ~/.claude_usage.json to change limits", color="#475569", size=10)
    print("---")

    # ── Today detail ──────────────────────────────────────────────────────────
    ln("TODAY", color="#F8FAFC", font="Helvetica-Bold", size=10)
    if t_out:
        ln(f"↑ {fmt_tokens(t_in)} input   ↓ {fmt_tokens(t_out)} output",
           color="#CBD5E1", size=12)
        if t_cr or t_cw:
            ln(f"📦 {fmt_tokens(t_cr)} cache read  ·  {fmt_tokens(t_cw)} written",
               color="#94A3B8", size=12)
        ln(fmt_cost(t_in, t_out, cfg) + " estimated", color="#86EFAC", size=12)
    else:
        ln("No activity today", color="#64748B", size=12)
    print("---")

    if y_in + y_out:
        ln(f"Yesterday   {fmt_tokens(y_in+y_out)} tokens   {fmt_cost(y_in, y_out, cfg)}",
           color="#94A3B8", size=12)
        print("---")

    ln("LAST 7 DAYS", color="#F8FAFC", font="Helvetica-Bold", size=10)
    if total_week:
        ln(f"{fmt_tokens(total_week)} tokens   {fmt_cost(w_in, w_out, cfg)}",
           color="#CBD5E1", size=12)
    else:
        ln("No activity this week", color="#64748B", size=12)
    print("---")

    ln("LAST 30 DAYS", color="#F8FAFC", font="Helvetica-Bold", size=10)
    if total_month:
        ln(f"{fmt_tokens(total_month)} tokens   {fmt_cost(m_in, m_out, cfg)}",
           color="#CBD5E1", size=12)
    else:
        ln("No activity this month", color="#64748B", size=12)
    print("---")

    # ── Top projects ──────────────────────────────────────────────────────────
    ln("PROJECTS", color="#F8FAFC", font="Helvetica-Bold", size=10)
    if by_project:
        ranked = sorted(by_project.items(), key=lambda kv: kv[1]["in"] + kv[1]["out"], reverse=True)
        for proj_path, tok in ranked[:5]:
            total = tok["in"] + tok["out"]
            if total == 0:
                continue
            name = project_name(proj_path)
            ln(f"{fmt_tokens(total)}   {name}", color="#94A3B8", size=11)
        if not CLAUDE_DIR.exists():
            ln("~/.claude/projects not found", color="#EF4444", size=12)
        else:
            ln(f"{len(by_project)} project(s)   {n_files} session file(s)",
               color="#475569", size=10)
    else:
        ln("No projects found", color="#64748B", size=12)
    print("---")

    # ── Optional API usage ────────────────────────────────────────────────────
    api = fetch_api_usage()
    if api and "_error" not in api:
        data    = api.get("data") or []
        api_in  = sum(int(d.get("input_tokens") or 0) for d in data)
        api_out = sum(int(d.get("output_tokens") or 0) for d in data)
        ln("API USAGE (30d)", color="#F8FAFC", font="Helvetica-Bold", size=10)
        ln(f"{fmt_tokens(api_in+api_out)} tokens   {fmt_cost(api_in, api_out, cfg)}",
           color="#CBD5E1", size=12)
        print("---")
    elif API_KEY:
        ln("API key set — usage endpoint unavailable", color="#64748B", size=11)
        print("---")
    else:
        ln("Set ANTHROPIC_API_KEY for org-level API usage", color="#64748B", size=11)
        print("---")

    ln(f"Updated {now.strftime('%H:%M:%S')}   plan: {cfg['plan']}",
       color="#475569", size=10)
    ln("Refresh now", refresh="true", color="#64748B", size=11)


if __name__ == "__main__":
    main()
