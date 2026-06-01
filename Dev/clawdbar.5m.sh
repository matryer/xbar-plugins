#!/usr/bin/env bash
#
# <xbar.title>clawdbar</xbar.title>
# <xbar.version>v2.0</xbar.version>
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

# --- Call the official usage endpoint ---------------------------------------
RESP=$(/usr/bin/curl -s --max-time 10 https://api.anthropic.com/api/oauth/usage \
  -H "Authorization: Bearer $TOKEN" \
  -H "anthropic-beta: oauth-2025-04-20" \
  -H "User-Agent: claude-code/$CC_VER" \
  -H "Accept: application/json")

# --- Render -----------------------------------------------------------------
RESP="$RESP" "$PY" <<'PYEOF'
import os, sys, json, io, base64
from datetime import datetime, timezone
from PIL import Image, ImageDraw, ImageFont

raw = os.environ.get("RESP", "")
try:
    d = json.loads(raw)
except Exception:
    print("Claude ❓")
    print("---")
    print("Bad API response (probably rate-limited 429). Will retry.")
    sys.exit(0)

if "five_hour" not in d or d.get("five_hour") is None:
    print("Claude ❓")
    print("---")
    err = d.get("error")
    msg = err.get("message") if isinstance(err, dict) else str(d)[:80]
    print("No usage data: " + str(msg)[:80])
    sys.exit(0)

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
def draw_icon(fh, sd):
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
    buf = io.BytesIO()
    img.save(buf, "PNG", dpi=(144, 144))
    return base64.b64encode(buf.getvalue()).decode()

fh = d["five_hour"].get("utilization") or 0
sd = d["seven_day"].get("utilization") or 0

print("| image=%s" % draw_icon(fh, sd))

print("---")
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
