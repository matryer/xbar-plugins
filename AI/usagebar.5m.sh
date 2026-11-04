#!/usr/bin/env bash
# <xbar.title>UsageBar</xbar.title>
# <xbar.version>v0.1.1</xbar.version>
# <xbar.author>JayJay1024</xbar.author>
# <xbar.author.github>JayJay1024</xbar.author.github>
# <xbar.desc>Shows your Claude plan usage (current 5-hour session and this week) with reset times. A single read-only bash script: reads Claude Code's OAuth token from the Keychain without ever refreshing or writing it, and only talks to api.anthropic.com. Unofficial, not affiliated with Anthropic.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/JayJay1024/usagebar-for-claude/v0.1.0/docs/images/menu-light.png</xbar.image>
# <xbar.dependencies>bash,curl,jq,Claude Code (signed in)</xbar.dependencies>
# <xbar.abouturl>https://github.com/JayJay1024/usagebar-for-claude</xbar.abouturl>
# <xbar.var>string(USAGEBAR_KEYCHAIN_SERVICE="Claude Code-credentials"): Keychain item that holds the Claude Code credentials.</xbar.var>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
#
# UsageBar — SwiftBar / xbar plugin.
#
# Security notes:
#   * Reads the Claude Code OAuth token from the macOS Keychain, read-only.
#   * Never refreshes the token and never writes anything back (refresh-token
#     rotation could otherwise sign Claude Code out).
#   * The token is only ever sent to https://api.anthropic.com, passed to curl
#     through stdin so it does not show up in the process list, and is never
#     logged or written to disk.
#
# Optional environment variables:
#   USAGEBAR_KEYCHAIN_SERVICE  Keychain item name (default: "Claude Code-credentials")
#   USAGEBAR_JQ                Path to jq (default: auto-detect)
#   USAGEBAR_FIXTURE           Path to a JSON response to render instead of calling the API (testing)
#   USAGEBAR_NOW               Unix timestamp to use as "now" (testing)

set -u

readonly API_URL="https://api.anthropic.com/api/oauth/usage"
readonly USAGE_PAGE="https://claude.ai/settings/usage"
readonly KEYCHAIN_SERVICE="${USAGEBAR_KEYCHAIN_SERVICE:-Claude Code-credentials}"

# SwiftBar/xbar run plugins with a minimal PATH, so look in the usual places.
find_jq() {
  if [ -n "${USAGEBAR_JQ:-}" ]; then echo "$USAGEBAR_JQ"; return; fi
  local p
  for p in "$(command -v jq 2>/dev/null)" /usr/bin/jq /opt/homebrew/bin/jq /usr/local/bin/jq; do
    if [ -n "$p" ] && [ -x "$p" ]; then echo "$p"; return; fi
  done
}

# Print an error state in SwiftBar format and exit successfully (so the menu still renders).
fail() {
  echo "UsageBar ⚠︎"
  echo "---"
  echo "$1"
  [ -n "${2:-}" ] && echo "$2"
  echo "---"
  echo "Refresh | refresh=true"
  echo "Open claude.ai usage page | href=$USAGE_PAGE"
  exit 0
}

JQ="$(find_jq)"
[ -n "$JQ" ] || fail "jq not found" "Install it with: brew install jq"

NOW="${USAGEBAR_NOW:-$(date +%s)}"

fetch_usage() {
  local cred token expires_at resp code
  cred="$(security find-generic-password -s "$KEYCHAIN_SERVICE" -w 2>/dev/null)" ||
    fail "Claude Code credentials not found" "Sign in by running: claude"

  token="$(printf '%s' "$cred" | "$JQ" -r '.claudeAiOauth.accessToken // empty' 2>/dev/null)"
  expires_at="$(printf '%s' "$cred" | "$JQ" -r '.claudeAiOauth.expiresAt // 0' 2>/dev/null)"
  unset cred
  [ -n "$token" ] || fail "No Claude OAuth token in Keychain" "Sign in by running: claude"

  # expiresAt is in milliseconds. Don't refresh it ourselves — let Claude Code do that.
  if [ "${expires_at%%.*}" -gt 0 ] 2>/dev/null && [ "$((NOW * 1000))" -ge "${expires_at%%.*}" ]; then
    fail "Claude token expired" "Run claude once to refresh it"
  fi

  resp="$(printf 'header = "Authorization: Bearer %s"\n' "$token" |
    curl -sS -m 15 -K - \
      -H "anthropic-beta: oauth-2025-04-20" \
      -H "Accept: application/json" \
      -w $'\n%{http_code}' \
      "$API_URL" 2>/dev/null)" || {
    unset token
    fail "Network error" "Could not reach api.anthropic.com"
  }
  unset token

  code="${resp##*$'\n'}"
  BODY="${resp%$'\n'*}"
  case "$code" in
    200) ;;
    401 | 403) fail "Claude token rejected (HTTP $code)" "Run claude once to refresh it" ;;
    429) fail "Rate limited by the usage API" "Will retry on the next refresh" ;;
    *) fail "Usage API returned HTTP $code" "The API may have changed — check for a UsageBar update" ;;
  esac
}

BODY=""
if [ -n "${USAGEBAR_FIXTURE:-}" ]; then
  BODY="$(cat "$USAGEBAR_FIXTURE")" || fail "Cannot read fixture" "$USAGEBAR_FIXTURE"
else
  fetch_usage
fi

# Render the API response as SwiftBar output.
# shellcheck disable=SC2016
RENDER='
def rep($s; $n): if $n <= 0 then "" else $s * $n end;
def pad2: tostring | if length < 2 then "0" + . else . end;
def epoch: if . == null then null
  else (sub("\\.[0-9]+"; "") | sub("\\+00:00$"; "Z") | try fromdateiso8601 catch null) end;
def rel($s):
  if $s <= 0 then "now"
  else ($s / 86400 | floor) as $d
     | (($s % 86400) / 3600 | floor) as $h
     | (($s % 3600) / 60 | floor) as $m
     | if $d > 0 then "\($d)d \($h)h"
       elif $h > 0 then "\($h)h \($m | pad2)m"
       else "\([$m, 1] | max)m" end
  end;
def pct: if . == null then null else round end;
def shown: if . == null then "–" else "\(.)%" end;
def color($p): if $p == null then null elif $p >= 90 then "#FF453A" elif $p >= 75 then "#FF9F0A" else null end;
def bar($p): if $p == null then rep("░"; 10)
  else ([[($p / 10 | round), 10] | min, 0] | max) as $f | rep("█"; $f) + rep("░"; 10 - $f) end;
def colorparam($p): color($p) // "" | if . == "" then "" else " color=" + . end;
# Lines with an action (refresh=true) render as enabled items in the primary label color.
# Lines without one render as disabled items in the system secondary color, which adapts
# to whatever is behind the translucent menu. Never hard-code a gray here: it becomes
# unreadable when the menu sits over a dark window.
def section($label; $w; $fmt):
  ($w.utilization | pct) as $p
  | ($w.resets_at | epoch | if . == null then null else ((. + 30) / 60 | floor) * 60 end) as $t
  | "\($label)  \($p | shown) used | size=13 refresh=true" + colorparam($p),
    "\(bar($p)) | font=Menlo size=12 refresh=true" + colorparam($p),
    (if $t != null then "Resets \($t | strflocaltime($fmt)) (in \(rel(($t - $now) | floor))) | size=12"
     elif ($p // 0) == 0 then "No active session | size=12"
     else "Reset time unknown | size=12" end);

(.five_hour.utilization | pct) as $s
| (.seven_day.utilization | pct) as $w
| ([$s, $w] | map(select(. != null)) | max) as $top
| "\($s | shown) · \($w | shown) | sfimage=gauge.medium" + colorparam($top),
  "---",
  section("Current session"; .five_hour; "%H:%M"),
  "---",
  section("This week"; .seven_day; "%a %H:%M"),
  "---",
  "Refresh | refresh=true",
  "Open claude.ai usage page | href=\($page)"
'

OUT="$(printf '%s' "$BODY" | "$JQ" -r \
  --argjson now "$NOW" --arg page "$USAGE_PAGE" \
  "$RENDER" 2>/dev/null)" || fail "Unexpected response from the usage API" "The API may have changed — check for a UsageBar update"
[ -n "$OUT" ] || fail "Empty response from the usage API"
printf '%s\n' "$OUT"
