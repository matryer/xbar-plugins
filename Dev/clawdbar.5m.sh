#!/usr/bin/env bash
#
# <xbar.title>clawdbar</xbar.title>
# <xbar.version>v2.2</xbar.version>
# <xbar.author>chao</xbar.author>
# <xbar.author.github>sunce764</xbar.author.github>
# <xbar.desc>Claude Code 5h/7d rate-limit usage as a mini progress bar in the menu bar, in Claude's official colors.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/sunce764/clawdbar/main/docs/demo.png</xbar.image>
# <xbar.dependencies>python3,curl,Pillow</xbar.dependencies>
# <xbar.abouturl>https://github.com/sunce764/clawdbar</xbar.abouturl>
#
# Refresh interval is set by the filename: clawdbar.5m.sh = every 5 minutes.
# The official usage endpoint rate-limits aggressively — do NOT go below ~5 minutes.

# --- Find a python3 that has Pillow installed -------------------------------
find_python() {
  for p in "$(command -v python3 2>/dev/null)" \
           /opt/homebrew/bin/python3 /usr/local/bin/python3 \
           /opt/miniconda3/bin/python3 "$HOME/miniconda3/bin/python3" \
           /usr/bin/python3; do
    [ -n "$p" ] && [ -x "$p" ] && "$p" -c "import PIL" 2>/dev/null && { echo "$p"; return 0; }
  done
  return 1
}
PY="$(find_python)"
if [ -z "$PY" ]; then
  echo "Claude ⚠️"
  echo "---"
  echo "No python3 with Pillow found. Install it with: pip3 install Pillow"
  exit 0
fi

# --- Claude Code version, for the User-Agent (avoids a stricter rate-limit bucket) ---
CC_VER=""
for c in "$(command -v claude 2>/dev/null)" /opt/homebrew/bin/claude "$HOME/.local/bin/claude"; do
  [ -n "$c" ] && [ -x "$c" ] && CC_VER=$("$c" --version 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1) && [ -n "$CC_VER" ] && break
done
[ -z "$CC_VER" ] && CC_VER="2.1.0"

# --- Read the Claude Code OAuth token from the macOS Keychain ---------------
CRED=$(/usr/bin/security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null)
if [ -z "$CRED" ]; then
  echo "Claude ⚠️"
  echo "---"
  echo "Can't read Keychain credentials."
  echo "On first run, click \"Always Allow\" in the prompt,"
  echo "or run \`claude\` once to log in."
  exit 0
fi

TOKEN=$(printf '%s' "$CRED" | "$PY" -c "import sys,json;print(json.load(sys.stdin)['claudeAiOauth']['accessToken'])" 2>/dev/null)

# --- Honor the macOS system proxy -------------------------------------------
# SwiftBar runs plugins with a minimal environment and does NOT inherit your
# shell's http(s)_proxy vars, so curl would connect directly. In regions where
# Anthropic blocks direct access (the API returns {"type":"forbidden",
# "message":"Request not allowed"}), that silently fails. So read the system
# proxy (set by a VPN / Clash / corporate config) and route curl through it,
# the same way Safari would. No proxy set -> PROXY_ARGS stays empty -> direct.
PROXY_ARGS=()
PX=$(/usr/sbin/scutil --proxy 2>/dev/null)
if printf '%s' "$PX" | grep -q "HTTPSEnable : 1"; then
  PHOST=$(printf '%s' "$PX" | awk '/HTTPSProxy/{print $3}')
  PPORT=$(printf '%s' "$PX" | awk '/HTTPSPort/{print $3}')
  [ -n "$PHOST" ] && [ -n "$PPORT" ] && PROXY_ARGS=(--proxy "http://$PHOST:$PPORT")
fi

# --- Call the official usage endpoint (with retries) ------------------------
# A single transient hiccup (network blip, proxy not ready right after wake,
# an occasional 429) shouldn't blank the menu bar. Retry up to 3x; use the
# first 200; on 429 stop early (don't hammer a rate-limit) and fall back to
# the cached value below instead of showing an error.
URL="https://api.anthropic.com/api/oauth/usage"
BODY="$(mktemp)"
RESP=""
for attempt in 1 2 3; do
  CODE=$(/usr/bin/curl -s -o "$BODY" -w "%{http_code}" --max-time 8 "${PROXY_ARGS[@]}" "$URL" \
    -H "Authorization: Bearer $TOKEN" \
    -H "anthropic-beta: oauth-2025-04-20" \
    -H "User-Agent: claude-code/$CC_VER" \
    -H "Accept: application/json")
  if [ "$CODE" = "200" ]; then RESP=$(cat "$BODY"); break; fi
  [ "$CODE" = "429" ] && break
  [ "$attempt" != "3" ] && sleep 2
done
rm -f "$BODY"

# Cache the last good response so a failed poll falls back to it (dimmed)
# instead of flipping the menu bar to an error glyph.
CACHE="$HOME/.cache/clawdbar/last.json"

# --- Render -----------------------------------------------------------------
RESP="$RESP" CACHE="$CACHE" "$PY" <<'PYEOF'
import os, sys, json, io, base64
from datetime import datetime, timezone
from PIL import Image, ImageDraw, ImageFont

raw   = os.environ.get("RESP", "")
cache = os.environ.get("CACHE", "")

def is_valid(d):
    return isinstance(d, dict) and isinstance(d.get("five_hour"), dict)

# 1) Parse this poll's response.
d = None
try:
    d = json.loads(raw)
except Exception:
    d = None

err_msg = None
if not is_valid(d):
    if isinstance(d, dict):
        e = d.get("error")
        err_msg = e.get("message") if isinstance(e, dict) else None
    d = None

# 2) This poll failed -> fall back to the last good cache (rendered dimmed),
#    so the menu bar keeps the last known value instead of an error glyph.
stale_secs = None
if d is None:
    try:
        with open(cache) as f:
            c = json.load(f)
        if is_valid(c.get("data")):
            d = c["data"]
            stale_secs = max(0, int(datetime.now().timestamp() - c.get("ts", 0)))
    except Exception:
        d = None

# 3) No fresh data AND no usable cache -> only now show an error.
if d is None:
    print("Claude ❓")
    print("---")
    if err_msg:
        print("No usage data: " + str(err_msg)[:80])
        if "not allowed" in str(err_msg).lower():
            print("Direct access may be geo-blocked — check your proxy/VPN is on | size=12 color=gray")
    else:
        print("Fetch failed (network / proxy / rate-limit). Will retry. | size=12 color=gray")
    sys.exit(0)

# 4) Got fresh data -> persist it as the fallback cache for next time.
if stale_secs is None and cache:
    try:
        os.makedirs(os.path.dirname(cache), exist_ok=True)
        with open(cache, "w") as f:
            json.dump({"data": d, "ts": datetime.now().timestamp()}, f)
    except Exception:
        pass

def parse_reset(iso):
    try:
        t = datetime.fromisoformat(iso)
        now = datetime.now(timezone.utc)
        local = t.astimezone()
        mins = int((t - now).total_seconds() // 60)
        if mins < 0:    rem = "now"
        elif mins < 60: rem = "in %dm" % mins
        else:           rem = "in %dh %dm" % (mins // 60, mins % 60)
        return local.strftime("%m-%d %H:%M"), rem
    except Exception:
        return iso, ""

# Official claude.ai progress-bar colors (sampled pixel-perfect from official screenshots)
def bar_rgb(p):
    if p >= 100: return (127, 44, 40, 255)   # dark red   #7F2C28  (rejected / capped)
    if p >= 70:  return (209, 147, 60, 255)  # gold       #D1933C  (approaching limit)
    return (58, 109, 203, 255)               # blue       #3A6DCB  (normal)

def bar_hex(p):
    if p >= 100: return "#7F2C28"
    if p >= 70:  return "#D1933C"
    return "#3A6DCB"

# Menu-bar icon: white text (matches other menu-bar items) + official-color bars.
# We draw an image because SwiftBar does NOT support 24-bit truecolor ANSI in titles,
# so exact colors like #3A6DCB are impossible with plain text.
def draw_icon(fh, sd, dim=False):
    S = 2                                    # retina 2x
    pad = 5*S; lab_w = 14*S; bar_w = 38*S; gap = 3*S; num_w = 30*S
    group_w = lab_w + gap + bar_w + gap + num_w
    ggap = 12*S
    W = pad*2 + group_w*2 + ggap
    H = 22*S
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    dr = ImageDraw.Draw(img)
    font = ImageFont.truetype("/System/Library/Fonts/Helvetica.ttc", 12*S)
    WHITE = (255, 255, 255, 255)
    def group(x, label, p):
        c = bar_rgb(p)
        dr.text((x, H/2), label, font=font, fill=WHITE, anchor="lm")
        bx = x + lab_w + gap; bh = 7*S; by = H/2; r = int(bh/2)
        dr.rounded_rectangle([bx, by-bh/2, bx+bar_w, by+bh/2], radius=r, fill=(255, 255, 255, 50))
        fw = max(bh, bar_w*min(p, 100)/100)
        dr.rounded_rectangle([bx, by-bh/2, bx+fw, by+bh/2], radius=r, fill=c)
        dr.text((bx+bar_w+gap, H/2), "%d%%" % round(p), font=font, fill=WHITE, anchor="lm")
    group(pad, "5h", fh)
    group(pad + group_w + ggap, "7d", sd)
    if dim:                                  # cached fallback -> dim it so stale data is obvious at a glance
        img.putalpha(img.split()[3].point(lambda v: int(v * 0.45)))
    buf = io.BytesIO()
    img.save(buf, "PNG", dpi=(144, 144))
    return base64.b64encode(buf.getvalue()).decode()

fh = d["five_hour"].get("utilization") or 0
sd = d["seven_day"].get("utilization") or 0

print("| image=%s" % draw_icon(fh, sd, dim=(stale_secs is not None)))

print("---")
if stale_secs is not None:
    mins = stale_secs // 60
    ago = "%dm ago" % mins if mins >= 1 else "%ds ago" % stale_secs
    print("⚠️ This poll failed — showing cached value (from %s) | size=12 color=#D1933C" % ago)
fh_t, fh_r = parse_reset(d["five_hour"]["resets_at"])
sd_t, sd_r = parse_reset(d["seven_day"]["resets_at"])
print("5-hour session: %.0f%% | color=%s" % (fh, bar_hex(fh)))
print("Resets %s (%s) | size=12 color=gray" % (fh_t, fh_r))
print("7-day (all models): %.0f%% | color=%s" % (sd, bar_hex(sd)))
print("Resets %s (%s) | size=12 color=gray" % (sd_t, sd_r))

eu = d.get("extra_usage") or {}
if eu.get("is_enabled"):
    used = eu.get("used_credits"); lim = eu.get("monthly_limit"); cur = eu.get("currency") or ""
    print("Extra usage: %s / %s %s | size=12 color=gray" % (used, lim, cur))

print("---")
print("Updated %s | size=11 color=gray" % datetime.now().strftime("%H:%M:%S"))
print("Refresh now | refresh=true")
PYEOF
