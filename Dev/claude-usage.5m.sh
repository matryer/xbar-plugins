#!/bin/bash
#
# SwiftBar/xbar plugin metadata (parsed by both plugin catalogs):
# <xbar.title>Claude Usage</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Henry Ng</xbar.author>
# <xbar.author.github>henryngcw</xbar.author.github>
# <xbar.desc>Live Claude Code subscription usage in the menu bar — the same numbers /usage reports, pulled from the keychain OAuth token. No API key, no setup.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/henryngcw/claude-usage-menubar/main/claude-code-usage-menu-bar-example.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/henryngcw/claude-usage-menubar</xbar.abouturl>
# <swiftbar.hideAbout>false</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>false</swiftbar.hideRunInTerminal>
#
# Claude Code usage in your macOS menu bar — a SwiftBar plugin.
# Shows your live subscription usage % and next reset time (same data as /usage).
#
# Requirements: macOS, SwiftBar, and Claude Code installed + logged in.
# No setup, no API key: the token is read live from your macOS keychain, so it
# always reflects whoever is logged into Claude Code on this machine.
#
# Claude meters a 5-hour (session) window and a 7-day (weekly) window — there is
# no literal "daily" limit, so "Session (5h)" is the day-to-day one.
# The ".5m." in the filename = refresh every 5 minutes. Rename to .30s./.1m./etc to taste.
#
# Note: this calls an undocumented endpoint that Claude Code uses internally.
# If it ever 401s, re-login to Claude Code; if it 404s, the path moved upstream.

export PATH="/usr/bin:/bin:$PATH"

# SwiftBar encodes the refresh interval in the filename (.5m.). The "Refresh
# interval" menu items below re-run us with this arg to rename the file; SwiftBar
# watches the folder and reschedules on the new name. ponytail: rename is the
# only knob SwiftBar gives — no runtime interval API.
SELF="${SWIFTBAR_PLUGIN_PATH:-$0}"
if [ "$1" = "--set-interval" ]; then
  mv "$SELF" "$(dirname "$SELF")/claude-usage.$2.sh"
  exit 0
fi
export SELF

# Read the OAuth token from the keychain (service name is the same for every user).
TOKEN=$(security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null \
  | /usr/bin/python3 -c 'import json,sys; print(json.load(sys.stdin)["claudeAiOauth"]["accessToken"])' 2>/dev/null)

# No token → empty body. Everything funnels through the same renderer below,
# which always shows the robot + 0% + "idle" when data is missing.
BODY=""
export STATUS="Not logged in — open Claude Code and sign in"
if [ -n "$TOKEN" ]; then
  STATUS="Usage endpoint unreachable"
  BODY=$(curl -s --max-time 8 \
    -H "Authorization: Bearer $TOKEN" \
    -H "anthropic-beta: oauth-2025-04-20" \
    -H "anthropic-version: 2023-06-01" \
    "https://api.anthropic.com/api/oauth/usage")
fi

echo "$BODY" | /usr/bin/python3 -c '
import json, os, re, sys
from datetime import datetime

# Parse whatever we got. Anything missing or an error reply -> no usage data,
# and we fall back to a single, consistent "0% · idle" UI.
status = os.environ.get("STATUS", "No usage data")
try:
    d = json.load(sys.stdin)
except Exception:
    d = {}
if not isinstance(d, dict) or "error" in d:
    status = (d.get("error") or {}).get("message", status) if isinstance(d, dict) else status
    d = {}

def reset(s, fmt):
    try:
        dt = datetime.fromisoformat(s).astimezone()   # -> local time
        return dt.strftime(fmt).replace(" 0", " ").lstrip("0")
    except Exception:
        return "idle"

fh, sd = d.get("five_hour") or {}, d.get("seven_day") or {}
fh_u, sd_u = fh.get("utilization", 0), sd.get("utilization", 0)
fh_r = reset(fh.get("resets_at", ""), "%I:%M %p")
sd_r = reset(sd.get("resets_at", ""), "%a %I:%M %p")

print(f"🤖 {fh_u:.0f}% · {fh_r}")      # menu bar: session % · next reset
print("---")
if not d:
    # No usage data. Explain why + how to recover, instead of a bare status line.
    print(f"⚠️ No usage data | size=12")
    print(f"{status} | size=11")
    print("---")
    print("How to fix: | size=11")
    if "logged in" in status.lower():
        print("1. Open Claude Code (the CLI) and run /login | size=11")
        print("2. Then click Refresh below | size=11")
    elif "rate" in status.lower():
        print("Rate-limited — wait a minute, then Refresh. | size=11")
        print("Do not poll faster than the .5m. filename. | size=11")
    else:
        print("1. Check your internet connection | size=11")
        print("2. In Claude Code, run /login to refresh the token | size=11")
        print("3. Click Refresh below | size=11")
    print("---")
print(f"Session (5h): {fh_u:.0f}%")
print(f"  resets {fh_r} | size=11")
print(f"Weekly (7d): {sd_u:.0f}%")
print(f"  resets {sd_r} | size=11")
print("---")
print("Refresh | refresh=true")

# Refresh-interval picker. Each item renames the script (via --set-interval) so
# SwiftBar reschedules. Current interval (parsed from our filename) gets a ✓.
self = os.environ.get("SELF", "")
cur = (re.search(r"\.(\d+[smh])\.sh$", os.path.basename(self)) or [None, ""])[1]
print("Refresh interval | size=11")
for label, val in [("2 min","2m"),("3 min","3m"),("5 min","5m"),
                   ("10 min","10m"),("30 min","30m"),("1 hour","1h")]:
    mark = " ✓" if val == cur else ""
    print(f"--{label}{mark} | bash=\"{self}\" param1=--set-interval param2={val} "
          f"terminal=false refresh=true | size=11")
print("--Shorter intervals can hit rate limits; 5 min is safe. | size=10")
'
