#!/bin/bash
# <xbar.title>Claude Pro Usage Monitor</xbar.title>
# <xbar.version>v5.0</xbar.version>
# <xbar.author>shabalingv-rgb</xbar.author>
# <xbar.author.github>shabalingv-rgb</xbar.author.github>
# <xbar.desc>Exact Claude Pro 5h session and weekly usage via Anthropic OAuth endpoint. No Node.js, no ccusage — pure Python. Shows utilization %, exact reset countdown, model breakdown, burn rate, weekly forecast.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/shabalingv-rgb/swiftbar-claude-usage/main/docs/screenshot.png</xbar.image>
# <xbar.dependencies>python3,Claude Code CLI</xbar.dependencies>
# <xbar.abouturl>https://github.com/shabalingv-rgb/swiftbar-claude-usage</xbar.abouturl>
# <swiftbar.title>Claude Pro Usage Monitor</swiftbar.title>
# <swiftbar.version>5.0</swiftbar.version>
# <swiftbar.author>shabalingv-rgb</swiftbar.author>
# <swiftbar.author.github>shabalingv-rgb</swiftbar.author.github>
# <swiftbar.refreshOnOpen>true</swiftbar.refreshOnOpen>
# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>false</swiftbar.hideLastUpdated>
#
# v5.0 — OAuth + JSONL параллельно. Нет ccusage, нет node.js.
#
#   Точный % берётся из Anthropic OAuth-эндпоинта (те же данные что на claude.ai).
#   JSONL читается параллельно для разбивки по моделям и темпа сжигания.
#   Итого: max(HTTP ~430ms, JSONL ~200ms) ≈ 430ms.
#
#   Источник %:  GET https://api.anthropic.com/api/oauth/usage
#   Токен:       macOS Keychain "Claude Code-credentials"
#   Fallback:    JSONL-вычисления (v4.0 алгоритм) если OAuth недоступен.

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

python3 << 'PYEOF'
import json, os, glob, sys, threading
import subprocess, urllib.request, urllib.error
from datetime import datetime, timezone, timedelta, date as _date

SESSION_COST_LIMIT = 10.0
WEEKLY_COST_FLOOR  = 20.0
BLOCK_DURATION     = timedelta(hours=5)
WEEK_RESET_DOW     = 4   # пятница

STATE_FILE    = os.path.expanduser("~/Library/Caches/com.swiftbar.claude-usage/state.json")
PROJECTS_GLOB = os.path.expanduser("~/.claude/projects/*/*.jsonl")

PRICING = {
    "opus":   {"in": 5.0, "out": 25.0, "cw5": 6.25, "cr": 0.50},
    "sonnet": {"in": 3.0, "out": 15.0, "cw5": 3.75, "cr": 0.30},
    "haiku":  {"in": 1.0, "out":  5.0, "cw5": 1.25, "cr": 0.10},
}
MODEL_ICON = {"opus": "🟣", "sonnet": "🔵", "haiku": "🟢"}

def family(m):
    m = (m or "").lower()
    for f in ("opus", "sonnet", "haiku"):
        if f in m: return f
    return "sonnet"

def load_state():
    try:
        with open(STATE_FILE) as f: return json.load(f)
    except Exception: return {}

def save_state(st):
    try:
        os.makedirs(os.path.dirname(STATE_FILE), exist_ok=True)
        with open(STATE_FILE, "w") as f: json.dump(st, f)
    except Exception: pass

now_utc = datetime.now(timezone.utc)
state   = load_state()

# ════════════════════════════════════════════════════════════════════
#  Параллельная загрузка: OAuth + JSONL
# ════════════════════════════════════════════════════════════════════
oauth_data = {}
jsonl_msgs = []   # (t, family, c_ncr, c_cr) последние 6h

def fetch_oauth():
    try:
        r = subprocess.run(
            ["security", "find-generic-password", "-s", "Claude Code-credentials", "-w"],
            capture_output=True, text=True, timeout=5
        )
        token = json.loads(r.stdout.strip())["claudeAiOauth"]["accessToken"]
        req = urllib.request.Request(
            "https://api.anthropic.com/api/oauth/usage",
            headers={
                "Authorization": f"Bearer {token}",
                "anthropic-beta": "oauth-2025-04-20",
                "User-Agent": "claude-code/2.0.37",
            }
        )
        with urllib.request.urlopen(req, timeout=8) as resp:
            oauth_data.update(json.loads(resp.read()))
    except Exception as e:
        oauth_data["_error"] = str(e)

def fetch_jsonl():
    cutoff = now_utc - timedelta(hours=6)
    seen   = set()
    for path in glob.glob(PROJECTS_GLOB):
        try:
            if datetime.fromtimestamp(os.path.getmtime(path), timezone.utc) < cutoff:
                continue
            fh = open(path, "r", errors="ignore")
        except OSError:
            continue
        with fh:
            for line in fh:
                if '"usage"' not in line: continue
                try: e = json.loads(line)
                except Exception: continue
                ts = e.get("timestamp")
                if not ts: continue
                try: t = datetime.fromisoformat(ts.replace("Z", "+00:00"))
                except Exception: continue
                if not (cutoff <= t <= now_utc): continue
                msg = e.get("message") or {}
                u   = msg.get("usage") or {}
                if not u: continue
                key = (msg.get("id"), e.get("requestId"))
                if key in seen: continue
                seen.add(key)
                fam = family(msg.get("model"))
                cc  = u.get("cache_creation") or {}
                i   = u.get("input_tokens", 0)
                o   = u.get("output_tokens", 0)
                cw  = (cc.get("ephemeral_1h_input_tokens", 0)
                     + cc.get("ephemeral_5m_input_tokens", 0)
                     or u.get("cache_creation_input_tokens", 0))
                p   = PRICING.get(fam, PRICING["sonnet"])
                c_ncr = (i*p["in"] + o*p["out"] + cw*p["cw5"]) / 1_000_000
                c_cr  = u.get("cache_read_input_tokens", 0) * p["cr"] / 1_000_000
                jsonl_msgs.append((t, fam, c_ncr, c_cr))

t1 = threading.Thread(target=fetch_oauth, daemon=True)
t2 = threading.Thread(target=fetch_jsonl, daemon=True)
t1.start(); t2.start()
t1.join(); t2.join()

jsonl_msgs.sort(key=lambda x: x[0])

# ════════════════════════════════════════════════════════════════════
#  OAuth: разбираем результат
# ════════════════════════════════════════════════════════════════════
oauth_ok = "_error" not in oauth_data and "five_hour" in oauth_data

block_active    = False
block_start     = None
block_resets_at = None
week_resets_at  = None
oauth_pct       = 0.0
oauth_7d_pct    = 0.0
time_str        = "—"

if oauth_ok:
    fh_data = oauth_data.get("five_hour") or {}
    wd_data = oauth_data.get("seven_day") or {}
    oauth_pct     = float(fh_data.get("utilization") or 0)
    oauth_7d_pct  = float(wd_data.get("utilization") or 0)
    resets_str    = fh_data.get("resets_at")
    wd_resets_str = wd_data.get("resets_at")

    if resets_str:
        try:
            block_resets_at = datetime.fromisoformat(resets_str)
            block_start     = block_resets_at - BLOCK_DURATION
            block_active    = now_utc < block_resets_at
        except Exception:
            pass

    if wd_resets_str:
        try:
            week_resets_at = datetime.fromisoformat(wd_resets_str)
        except Exception:
            pass

    if block_active and block_resets_at:
        rem_sec = int((block_resets_at - now_utc).total_seconds())
        rem_h, rem_m = rem_sec // 3600, (rem_sec % 3600) // 60
        time_str = f"{rem_h}ч {rem_m:02d}м" if rem_h > 0 else f"{rem_m}м"

# ════════════════════════════════════════════════════════════════════
#  JSONL: вычисляем разбивку по блоку
# ════════════════════════════════════════════════════════════════════
if oauth_ok and block_active and block_start:
    # OAuth дал нам точные границы блока — просто фильтруем JSONL
    fam_cost = {}
    jsonl_nc = 0.0
    for t, fam, c_ncr, _ in jsonl_msgs:
        if t < block_start: continue
        fam_cost[fam] = fam_cost.get(fam, 0.0) + c_ncr
        jsonl_nc += c_ncr
else:
    # Fallback: детектируем блоки самостоятельно (v4.0)
    blks = []
    for t, fam, c_ncr, c_cr in jsonl_msgs:
        if not blks or t >= blks[-1]["end"]:
            blks.append({"start": t, "end": t + BLOCK_DURATION,
                         "nc": 0.0, "fam_cost": {}})
        b = blks[-1]
        b["nc"] += c_ncr
        b["fam_cost"][fam] = b["fam_cost"].get(fam, 0.0) + c_ncr
    ab = next((b for b in reversed(blks) if b["start"] <= now_utc < b["end"]), None)
    if ab:
        jsonl_nc    = ab["nc"]
        fam_cost    = ab["fam_cost"]
        block_start = ab["start"]
        block_resets_at = ab["end"]
        block_active = True
        oauth_pct = jsonl_nc / SESSION_COST_LIMIT * 100
        rem_sec = int((ab["end"] - now_utc).total_seconds())
        rem_h, rem_m = rem_sec // 3600, (rem_sec % 3600) // 60
        time_str = f"{rem_h}ч {rem_m:02d}м" if rem_h > 0 else f"{rem_m}м"
    else:
        block_active = False
        jsonl_nc = 0.0
        fam_cost = {}

# Темп за последний час
hour_ago = now_utc - timedelta(hours=1)
nc_1h = sum(c_ncr for t, _, c_ncr, _ in jsonl_msgs
            if t >= hour_ago and (block_start is None or t >= block_start))

# Обновляем пик
if block_active and jsonl_nc > 0:
    state["peak_cost"] = max(state.get("peak_cost", 0.0), jsonl_nc)
    save_state(state)

# ════════════════════════════════════════════════════════════════════
#  Нет активного блока
# ════════════════════════════════════════════════════════════════════
if not block_active:
    src = "OAuth" if oauth_ok else "JSONL"
    print(f"🤖 –")
    print("---")
    print(f"Нет активной сессии · {src} | color=#888888")
    if oauth_ok:
        pct7 = oauth_7d_pct
        wi   = "🔴" if pct7 >= 85 else ("🟡" if pct7 >= 60 else "🟢")
        wr_str = ""
        if week_resets_at:
            wr_l = week_resets_at.astimezone()
            wr_str = f" · сброс {wr_l.strftime('%d %b %H:%M')}"
        print(f"{wi} Неделя: {pct7:.0f}%{wr_str} | size=12 color=#888888")
    else:
        wc = state.get("week_cost")
        if wc is not None:
            wl = max(state.get("peak_week_cost", 0.0), WEEKLY_COST_FLOOR)
            wp = wc / wl * 100 if wl else 0
            wi = "🔴" if wp >= 85 else ("🟡" if wp >= 60 else "🟢")
            print(f"{wi} Неделя: ${wc:.2f} / ${wl:.2f} ({wp:.0f}%) | size=12 color=#888888")
    print(f"Пик блока: ${state.get('peak_cost', 0):.2f} | size=12 color=#888888")
    print("---")
    print("Обновить | refresh=true")
    sys.exit(0)

# ════════════════════════════════════════════════════════════════════
#  Вывод: активная сессия
# ════════════════════════════════════════════════════════════════════
pct      = float(oauth_pct)
pct_disp = min(round(pct), 100)
over     = pct > 100
icon     = "🔴" if pct >= 85 else ("🟡" if pct >= 60 else "🟢")
src_lbl  = "OAuth" if oauth_ok else "JSONL"

print(f"{icon} {pct_disp}%{'+' if over else ''} · {time_str}")
print("---")

# ── Текущий блок ───────────────────────────────────────────────────
fam_sorted = sorted(fam_cost, key=lambda f: -fam_cost[f])
models_str = " · ".join(MODEL_ICON.get(f, "⚪️") + " " + f.capitalize()
                         for f in fam_sorted) if fam_sorted else "—"
bar_n = min(round(pct / 10), 10)
bar   = "█" * bar_n + "░" * (10 - bar_n)
warn  = " ⚠️ рекорд!" if over else ""
bs_local = block_start.astimezone().strftime("%H:%M") if block_start else "?"

print(f"Блок с {bs_local}  {models_str}  [{src_lbl}] | size=13 color=#888888")
print(f"  {pct:.0f}%  ≈${jsonl_nc:.2f} | size=13")
print(f"  [{bar}]{warn} | size=11 font=Menlo")
print(f"  До сброса: {time_str} | size=12")
print("---")

# ── Разбивка по моделям (из JSONL) ────────────────────────────────
if fam_cost:
    total_nc = sum(fam_cost.values()) or 1
    print("По моделям (≈) | size=12 color=#888888")
    for f in fam_sorted:
        if fam_cost[f] < 0.005: continue
        share = fam_cost[f] / total_nc * 100
        print(f"  {MODEL_ICON.get(f,'⚪️')} {f.capitalize():<7} ${fam_cost[f]:5.2f}  {share:3.0f}% | size=12 font=Menlo")
    print("---")

# ── Прогноз ────────────────────────────────────────────────────────
print("Прогноз | size=12 color=#888888")
if pct >= 100:
    print("  🔴 Лимит достигнут | size=12 color=#cc4444")
elif nc_1h > 0.01:
    rem_frac = (100.0 - pct) / 100.0
    rem_nc   = rem_frac * SESSION_COST_LIMIT
    eta_m    = rem_nc / nc_1h * 60
    mh, mm2  = int(eta_m // 60), int(eta_m % 60)
    eta_str  = f"{mh}ч {mm2:02d}м" if mh > 0 else f"{mm2}м"
    col = "#cc4444" if eta_m < 60 else "#888888"
    print(f"  Темп ≈${nc_1h:.2f}/ч → упор через ~{eta_str} | size=12 color={col}")
else:
    if block_start:
        elapsed_h = max((now_utc - block_start).total_seconds() / 3600, 0.1)
        avg_rate  = jsonl_nc / elapsed_h if jsonl_nc > 0 else 0
    else:
        avg_rate = 0
    if avg_rate > 0.01:
        rem_nc  = (100.0 - pct) / 100.0 * SESSION_COST_LIMIT
        eta_m   = rem_nc / avg_rate * 60
        mh, mm2 = int(eta_m // 60), int(eta_m % 60)
        eta_str = f"{mh}ч {mm2:02d}м" if mh > 0 else f"{mm2}м"
        print(f"  Ср.темп ≈${avg_rate:.2f}/ч → упор через ~{eta_str} | size=12 color=#888888")
    else:
        print("  Активность низкая | size=12 color=#888888")
print("---")

# ── Недельный лимит ────────────────────────────────────────────────
print("Неделя (7 дн) | size=12 color=#888888")
if oauth_ok:
    pct7   = float(oauth_7d_pct)
    bar7_n = min(round(pct7 / 10), 10)
    bar7   = "█" * bar7_n + "░" * (10 - bar7_n)
    wicon  = "🔴" if pct7 >= 85 else ("🟡" if pct7 >= 60 else "🟢")
    peak_wc = state.get("peak_week_cost", 0.0)
    est_str = f"  ≈${pct7/100*peak_wc:.1f} / ~${peak_wc:.0f}" if peak_wc > 0 else ""
    wr_local_str = ""
    if week_resets_at:
        wr_l = week_resets_at.astimezone()
        wr_local_str = f"  · сброс {wr_l.strftime('%a %d %b %H:%M')}"
    print(f"  {wicon} {pct7:.0f}%{est_str}{wr_local_str} | size=12")
    print(f"  [{bar7}] | size=11 font=Menlo")
    if pct7 > 0:
        days_since = (_date.today().weekday() - WEEK_RESET_DOW) % 7
        days_done  = max(days_since, 1)
        days_left  = max(6 - days_since, 0)
        proj_pct   = pct7 + (pct7 / days_done) * days_left
        dword = {0:"посл. день (чт)", 1:"+1 день до чт", 2:"+2 дня",
                 3:"+3 дня", 4:"+4 дня", 5:"+5 дней", 6:"+6 дней"}.get(days_left, f"+{days_left}д")
        if proj_pct >= 100:
            p_str = f"{proj_pct:.0f}% — лимит!"; p_col = "#cc4444"
        elif proj_pct >= 80:
            p_str = f"{proj_pct:.0f}% — осторожно"; p_col = "#cc8800"
        else:
            p_str = f"{proj_pct:.0f}% — хватит"; p_col = "#44aa44"
        print(f"  К чт ({dword}): {p_str} | size=12 color={p_col}")
else:
    wc = state.get("week_cost")
    if wc is not None:
        wl = max(state.get("peak_week_cost", 0.0), WEEKLY_COST_FLOOR)
        wp = wc / wl * 100 if wl else 0
        wi = "🔴" if wp >= 85 else ("🟡" if wp >= 60 else "🟢")
        ts_ = state.get("week_updated", "")[:16].replace("T", " ")
        print(f"  {wi} ${wc:.2f} / ${wl:.2f} ({wp:.0f}%) · {ts_} UTC | size=12")
        proj = state.get("cal_projected_eow", 0)
        if proj > 0 and wl:
            print(f"  К чт: ≈${proj:.1f} ({proj/wl*100:.0f}% лимита) | size=12 color=#888888")
    else:
        print("  Нет данных | size=12 color=#888888")
print("---")

# ── История сессий из JSONL (6ч окно чтения) ───────────────────────
blk_hist = []
for t, fam, c_ncr, _ in jsonl_msgs:
    if not blk_hist or t >= blk_hist[-1]["end"]:
        blk_hist.append({"start": t, "end": t + BLOCK_DURATION,
                          "nc": 0.0, "fam_cost": {}})
    b = blk_hist[-1]
    b["nc"] += c_ncr
    b["fam_cost"][fam] = b["fam_cost"].get(fam, 0.0) + c_ncr

if len(blk_hist) > 1:
    print("Сессии (6ч) | size=12 color=#888888")
    for b in blk_hist:
        sl  = b["start"].astimezone()
        cur = block_start and abs((b["start"] - block_start).total_seconds()) < 300
        lbl = "▶ сейчас" if cur else sl.strftime("%H:%M")
        bp  = min(round(b["nc"] / SESSION_COST_LIMIT * 100), 999)
        fms = "".join(MODEL_ICON.get(f, "⚪️")
                      for f in sorted(b["fam_cost"], key=lambda x: -b["fam_cost"][x]))
        flg = " ⚠️" if b["nc"] >= SESSION_COST_LIMIT * 0.95 else ""
        print(f"  {lbl:<9} ≈${b['nc']:4.2f}  {bp:>3}% {fms}{flg} | size=12 font=Menlo")
    print("---")

# ── Подвал ─────────────────────────────────────────────────────────
err = f" · err: {oauth_data.get('_error','')[:35]}" if not oauth_ok else ""
print(f"v5.0 {src_lbl}+JSONL · пик ${state.get('peak_cost',0):.2f}{err} | size=11 color=#888888")
print("Обновить | refresh=true")
print("📊 Записать точку данных... | bash=/usr/bin/python3 param1=/Users/grigorijsabalin/.claude-usage-calibrate/collect.py terminal=false refresh=true")
PYEOF
