#!/usr/bin/env bash

# <xbar.title>Codex Reset</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>codex-reset.com</xbar.author>
# <xbar.author.github>suvadadepolo-blip</xbar.author.github>
# <xbar.desc>Chance that OpenAI resets Codex usage limits in the next 24/48 hours, the last confirmed reset, and Codex service status. Free public API, no key. Data: codex-reset.com</xbar.desc>
# <xbar.dependencies>curl</xbar.dependencies>
# <xbar.abouturl>https://codex-reset.com/developers</xbar.abouturl>
# <xbar.var>boolean(VAR_SHOW_48H=false): Show the 48-hour chance in the menu bar instead of the 24-hour one.</xbar.var>

# Global early resets only: your own 5-hour/weekly window is shown by /status in Codex.
# The API refreshes about once a minute; the default 10-minute refresh is plenty.

API="https://codex-reset.com"
UA="xbar-codex-reset/1.0 (+https://github.com/matryer/xbar-plugins)"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# plutil ships with macOS and reads JSON, so no jq is needed.
get() { plutil -extract "$2" raw -o - "$TMP/$1.json" 2>/dev/null; }

if ! curl -fsS --max-time 10 -A "$UA" "$API/api/forecast" -o "$TMP/forecast.json"; then
  echo "Codex ↻ ?"
  echo "---"
  echo "codex-reset.com is unreachable | color=red"
  echo "Refresh | refresh=true"
  exit 0
fi
curl -fsS --max-time 10 -A "$UA" "$API/api/status-history" -o "$TMP/status.json" 2>/dev/null

p24=$(get forecast probabilities.rounded_24h)
p48=$(get forecast probabilities.rounded_48h)
confidence=$(get forecast confidence)
last_reset=$(get forecast last_reset_at)
age_days=$(get forecast age_days)
signal=$(plutil -type official_signal "$TMP/forecast.json" 2>/dev/null)
codex_status=$(get status current.codex)

if [ "$VAR_SHOW_48H" = "true" ]; then
  echo "Codex ↻ ${p48:-?}%"
else
  echo "Codex ↻ ${p24:-?}%"
fi
echo "---"
echo "Chance of a global usage-limit reset"
echo "Next 24h: ${p24:-?}%    Next 48h: ${p48:-?}% | font=Menlo"
[ -n "$confidence" ] && echo "Confidence: $confidence | size=12"
if [ "$signal" = "dictionary" ]; then
  echo "Official reset signal active | color=#2da44e href=$API/"
fi
if [ -n "$last_reset" ]; then
  echo "Last confirmed reset: ${last_reset%%T*} (${age_days%%.*} days ago) | href=$API/timeline"
fi
if [ -n "$codex_status" ]; then
  if [ "$codex_status" = "operational" ]; then
    echo "Codex service: operational | href=$API/status"
  else
    echo "Codex service: $codex_status | color=red href=$API/status"
  fi
fi
echo "---"
echo "A forecast, not a promise. Your own window: /status in Codex | size=11 disabled=true"
echo "Open codex-reset.com | href=$API/"
echo "Reset history | href=$API/timeline"
echo "Data: codex-reset.com | href=$API/developers size=11"
echo "Refresh | refresh=true"
