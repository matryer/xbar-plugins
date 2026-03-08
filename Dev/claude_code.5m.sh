#!/usr/bin/env bash
#<xbar.title>Claude Usage</xbar.title>
#<xbar.version>1.0</xbar.version>
#<xbar.author>Rodrigo Nemmen da Silva</xbar.author>
#<xbar.desc>Display Claude Code API rate limit utilization</xbar.desc>
#<xbar.dependencies>curl,python3</xbar.dependencies>
#<xbar.image>https://raw.githubusercontent.com/rsnemmen/claude-usage/refs/heads/main/SCR-20260219-jges.png</xbar.image>

# User variables
# ================
#<xbar.var>boolean(VAR_SHOW_7D="false"): Also show 7-day window in title (e.g. 45%/23%).</xbar.var>
#<xbar.var>boolean(VAR_COLORS="true"): Color-code title at warning (>75%) and critical (>90%) levels.</xbar.var>
#<xbar.var>boolean(VAR_SHOW_RESET="true"): Show time-until-reset for each window in the dropdown.</xbar.var>
#<xbar.var>boolean(VAR_SHOW_BARS="true"): Show dynamic dual progress bar icon (5h top, 7d bottom) instead of the Claude logo.</xbar.var>

SHOW_7D="${VAR_SHOW_7D:-false}"
COLORS="${VAR_COLORS:-true}"
SHOW_RESET="${VAR_SHOW_RESET:-true}"
SHOW_BARS="${VAR_SHOW_BARS:-true}"

CLAUDE_ICON="iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAAXNSR0IArs4c6QAAAHhlWElmTU0AKgAAAAgABAEaAAUAAAABAAAAPgEbAAUAAAABAAAARgEoAAMAAAABAAIAAIdpAAQAAAABAAAATgAAAAAAAABIAAAAAQAAAEgAAAABAAOgAQADAAAAAQABAACgAgAEAAAAAQAAABKgAwAEAAAAAQAAABIAAAAAqSaGYgAAAAlwSFlzAAALEwAACxMBAJqcGAAAAdJJREFUOBGV0z1IVWEYB3Cv2ZAVlQUpWDnYJqZBREPU1tISBo1OBkEfWBGNQhTR1iwu2hIENdYUVBRBBjXVUEZRYBLah2CD3X5/O/cQ14vRA7/zPO/Hec857zmnqakuqtVqM11U6ob+r2mBQ3zjYu1MdR/3aKv1/TObvI9ffKqdKA+zwOYsIPewv+FiBrbSWky8oU6cKtrX1C+Kuludi3xkaX65oI4WJnlOLx185TUZmyi0yk9JXGf5Puo8zjzfGeACiSPcJu0xErno+vJO6guDu3hAYpwZnvCY3G1ijp76cxu2TTxP7qwWi0WRlzBCP0OMcou2isMWq43wky/MME0vZ9lELaqKjK0m8/ICHjLW4jDPSzrYQDt9JBb/pPKYua+4zyTvmapUKgvy8nCXnWSPpsjj1GJKcZm7TJN4R3vuqAwd6zSGyfdzh2fkEbrIFkQ3A+Tx+lnLbPkdWGSvjnFWcYJOhhjlMD84wCO2c9QjfZaXorlWyLlCFtrNLJcYZAdznCPfzgcmyLe1U24cBrfxhmOZIednvVrUg+rEHvJzH0x/wzB4hisZlNeQ/+p00c7ncpOTDU/+u9Ok8gWoN/KW7FEZ2n9vSdm/YuGkdrJ/K8ZvZcjUYTq3RuAAAAAASUVORK5CYII="

# === Helper: show error with logo and warning ===

show_error() {
  local message="$1"
  echo "! | templateImage=${CLAUDE_ICON}"
  echo "---"
  echo "${message}"
  echo "---"
  echo "Refresh | refresh=true"
  exit 0
}

USAGE_CACHE="/tmp/.claude_swiftbar_cache"
TOKEN_CACHE="/tmp/.claude_swiftbar_token"
CACHE_TTL=300   # 5 minutes — matches poll interval
TOKEN_TTL=900   # 15 minutes

# === Get token (cached) ===

TOKEN=""
if [ -f "$TOKEN_CACHE" ]; then
  cache_age=$(( $(date -u +%s) - $(stat -f %m "$TOKEN_CACHE" 2>/dev/null || echo 0) ))
  if [ "$cache_age" -lt "$TOKEN_TTL" ]; then
    TOKEN="$(cat "$TOKEN_CACHE" 2>/dev/null)"
  fi
fi

if [ -z "$TOKEN" ]; then
  RAW_CREDS="$(security find-generic-password -s "Claude Code-credentials" -w 2>/dev/null)"
  if [ -z "$RAW_CREDS" ]; then
    show_error "No Claude Code credentials found in Keychain. Sign in to Claude Code first."
  fi
  TOKEN="$(printf '%s' "$RAW_CREDS" | python3 -c "
import json, sys
try:
    d = json.loads(sys.stdin.read().strip())
    if 'claudeAiOauth' in d:
        print(d['claudeAiOauth']['accessToken'])
    elif 'accessToken' in d:
        print(d['accessToken'])
    else:
        sys.exit(1)
except Exception:
    sys.exit(1)
" 2>/dev/null)"
  if [ -z "$TOKEN" ]; then
    show_error "Could not parse Claude Code credentials."
  fi
  printf '%s' "$TOKEN" > "$TOKEN_CACHE"
fi

# === Load usage from cache or fetch from API ===

parsed=""

if [ -f "$USAGE_CACHE" ]; then
  cache_age=$(( $(date -u +%s) - $(stat -f %m "$USAGE_CACHE" 2>/dev/null || echo 0) ))
  if [ "$cache_age" -lt "$CACHE_TTL" ]; then
    parsed="$(cat "$USAGE_CACHE" 2>/dev/null)"
  fi
fi

if [ -z "$parsed" ]; then
  response="$(curl -s --connect-timeout 5 --max-time 10 -w "\n%{http_code}" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H "anthropic-beta: oauth-2025-04-20" \
    -H "Accept: application/json" \
    "https://api.anthropic.com/api/oauth/usage")"

  http_code="$(printf '%s\n' "$response" | tail -n 1)"
  body="$(printf '%s\n' "$response" | sed '$d')"

  if [ -z "$http_code" ] || [ "$http_code" = "000" ]; then
    show_error "No internet connection."
  fi

  if [ "$http_code" = "401" ]; then
    # Token may be stale — clear cache so next run re-reads from Keychain
    rm -f "$TOKEN_CACHE"
    show_error "Token expired. Please sign in to Claude Code again."
  elif [ "$http_code" = "429" ]; then
    show_error "Usage API rate limited. Will retry shortly."
  elif [ "$http_code" -lt 200 ] || [ "$http_code" -ge 300 ]; then
    show_error "API error: HTTP $http_code"
  fi

  parsed="$(printf '%s' "$body" | python3 -c "
import json, sys
try:
    d = json.loads(sys.stdin.read())
    def get_val(window, field, default='0'):
        try:
            w = d.get(window)
            if not w:
                return default
            v = w.get(field)
            return str(v) if v is not None else default
        except Exception:
            return default
    print(get_val('five_hour',      'utilization', '0'))
    print(get_val('seven_day',      'utilization', '0'))
    print(get_val('seven_day_opus', 'utilization', '0'))
    print(get_val('five_hour',      'resets_at',   ''))
    print(get_val('seven_day',      'resets_at',   ''))
    print(get_val('seven_day_opus', 'resets_at',   ''))
except Exception as e:
    sys.stderr.write(str(e) + '\n')
    sys.exit(1)
" 2>/dev/null)"

  if [ -z "$parsed" ]; then
    show_error "Could not parse API response"
  fi

  printf '%s\n' "$parsed" > "$USAGE_CACHE"
fi

UTIL_5H="$(      printf '%s\n' "$parsed" | sed -n '1p')"
UTIL_7D="$(      printf '%s\n' "$parsed" | sed -n '2p')"
UTIL_7D_OPUS="$( printf '%s\n' "$parsed" | sed -n '3p')"
RESET_5H="$(     printf '%s\n' "$parsed" | sed -n '4p')"
RESET_7D="$(     printf '%s\n' "$parsed" | sed -n '5p')"
RESET_7D_OPUS="$(printf '%s\n' "$parsed" | sed -n '6p')"

format_pct() {
  python3 -c "print(round(float('${1:-0}')))" 2>/dev/null || echo "0"
}

PCT_5H="$(      format_pct "$UTIL_5H")"
PCT_7D="$(      format_pct "$UTIL_7D")"
PCT_7D_OPUS="$( format_pct "$UTIL_7D_OPUS")"

# === Helper: human-readable countdown from ISO 8601 timestamp ===

time_until() {
  local ts="$1"
  [ -z "$ts" ] && echo "?" && return
  python3 -c "
from datetime import datetime, timezone
ts = '${ts}'
try:
    if ts.endswith('Z'):
        ts = ts[:-1] + '+00:00'
    reset = datetime.fromisoformat(ts)
    now = datetime.now(timezone.utc)
    diff = reset - now
    secs = diff.total_seconds()
    if secs <= 0:
        print('now')
    else:
        days  = int(secs // 86400)
        hours = int((secs % 86400) // 3600)
        mins  = int((secs % 3600) // 60)
        if days > 0:
            print(f'{days}d {hours}h')
        elif hours > 0:
            print(f'{hours}h {mins}m')
        else:
            print(f'{mins}m')
except Exception:
    print('?')
" 2>/dev/null || echo "?"
}

# === Helper: color for a given percentage ===

color_for_pct() {
  local pct=$1
  if [ "$COLORS" = "true" ]; then
    [ "$pct" -ge 90 ] 2>/dev/null && echo "#FF0000" && return
    [ "$pct" -ge 75 ] 2>/dev/null && echo "#FFD700" && return
  fi
  echo ""
}

# === Helper: ASCII progress bar (20 chars) ===

make_bar() {
  local pct="${1:-0}"
  local width=20
  local filled
  filled=$(python3 -c "print(min(int(round(${pct} * ${width} / 100)), ${width}))" 2>/dev/null || echo "0")
  local bar=""
  local i=1
  while [ "$i" -le "$width" ]; do
    if [ "$i" -le "$filled" ]; then
      bar="${bar}█"
    else
      bar="${bar}░"
    fi
    i=$((i + 1))
  done
  echo "$bar"
}

# === Helper: dynamic dual progress bar icon ===

make_icon() {
  local pct5h="${1:-0}" pct7d="${2:-0}"
  python3 -c "
import struct, zlib, base64

def color_for(pct):
    return (0, 0, 0, 255)

def make_png(w, h, rows_rgba):
    def chunk(tag, data):
        c = struct.pack('>I', len(data)) + tag + data
        return c + struct.pack('>I', zlib.crc32(c[4:]) & 0xffffffff)
    ihdr = struct.pack('>IIBBBBB', w, h, 8, 6, 0, 0, 0)
    raw = b''
    for row in rows_rgba:
        raw += b'\x00'
        for (r,g,b,a) in row:
            raw += bytes([r,g,b,a])
    idat = zlib.compress(raw)
    return (b'\x89PNG\r\n\x1a\n'
            + chunk(b'IHDR', ihdr)
            + chunk(b'IDAT', idat)
            + chunk(b'IEND', b''))

W, H = 32, 14
p5 = min(max(int(round(${pct5h})), 0), 100)
p7 = min(max(int(round(${pct7d})), 0), 100)
c5 = color_for(p5)
c7 = color_for(p7)
fill5 = int(round(p5 * W / 100))
fill7 = int(round(p7 * W / 100))
EMPTY = (0, 0, 0, 60)
CLEAR = (0, 0, 0, 0)

def bar_row(fill, fg):
    return [fg if x < fill else EMPTY for x in range(W)]

rows = []
for row_i in range(H):
    if 1 <= row_i <= 5:
        rows.append(bar_row(fill5, c5))
    elif 9 <= row_i <= 13:
        rows.append(bar_row(fill7, c7))
    else:
        rows.append([CLEAR] * W)

print(base64.b64encode(make_png(W, H, rows)).decode())
" 2>/dev/null
}

# === Build menu bar title ===

COLOR_5H="$(color_for_pct "$PCT_5H")"
COLOR_7D="$(color_for_pct "$PCT_7D")"

# For title, use the "most urgent" color (critical > warning > none)
title_color() {
  local c1="$1" c2="$2"
  [ "$c1" = "#FF0000" ] || [ "$c2" = "#FF0000" ] && echo "#FF0000" && return
  [ "$c1" = "#FFD700" ] || [ "$c2" = "#FFD700" ] && echo "#FFD700" && return
  echo ""
}

if [ "$SHOW_7D" = "true" ]; then
  TITLE_COLOR="$(title_color "$COLOR_5H" "$COLOR_7D")"
  TITLE="${PCT_5H}%/${PCT_7D}%"
else
  TITLE_COLOR="$COLOR_5H"
  TITLE="${PCT_5H}%"
fi

# Emit menu bar line
if [ "$SHOW_BARS" = "true" ]; then
  BAR_ICON="$(make_icon "$PCT_5H" "$PCT_7D")"
  if [ -n "$TITLE_COLOR" ]; then
    echo " | image=${BAR_ICON} color=${TITLE_COLOR}"
  else
    echo " | image=${BAR_ICON}"
  fi
else
  if [ -n "$TITLE_COLOR" ]; then
    echo "${TITLE} | templateImage=${CLAUDE_ICON} color=${TITLE_COLOR}"
  else
    echo "${TITLE} | templateImage=${CLAUDE_ICON}"
  fi
fi

# === Dropdown ===

echo "---"

# --- 5h window ---
BAR_5H="$(make_bar "$PCT_5H")"
if [ -n "$COLOR_5H" ]; then
  echo "5h window | color=#888888"
  echo "5h: ${PCT_5H}% ${BAR_5H} | color=${COLOR_5H}"
else
  echo "5h window | color=#888888"
  echo "5h: ${PCT_5H}% ${BAR_5H}"
fi

if [ "$SHOW_RESET" = "true" ] && [ -n "$RESET_5H" ]; then
  UNTIL_5H="$(time_until "$RESET_5H")"
  echo "Resets in: ${UNTIL_5H} | color=#888888"
fi

echo "---"

# --- 7d window ---
COLOR_7D_VAL="$(color_for_pct "$PCT_7D")"
BAR_7D="$(make_bar "$PCT_7D")"
echo "7d window | color=#888888"
if [ -n "$COLOR_7D_VAL" ]; then
  echo "7d: ${PCT_7D}% ${BAR_7D} | color=${COLOR_7D_VAL}"
else
  echo "7d: ${PCT_7D}% ${BAR_7D}"
fi

if [ "$SHOW_RESET" = "true" ] && [ -n "$RESET_7D" ]; then
  UNTIL_7D="$(time_until "$RESET_7D")"
  echo "Resets in: ${UNTIL_7D} | color=#888888"
fi

echo "---"

# --- 7d Opus window ---
COLOR_OPUS="$(color_for_pct "$PCT_7D_OPUS")"
BAR_OPUS="$(make_bar "$PCT_7D_OPUS")"
echo "7d Opus window | color=#888888"
if [ -n "$COLOR_OPUS" ]; then
  echo "7d Opus: ${PCT_7D_OPUS}% ${BAR_OPUS} | color=${COLOR_OPUS}"
else
  echo "7d Opus: ${PCT_7D_OPUS}% ${BAR_OPUS}"
fi

if [ "$SHOW_RESET" = "true" ] && [ -n "$RESET_7D_OPUS" ]; then
  UNTIL_OPUS="$(time_until "$RESET_7D_OPUS")"
  echo "Resets in: ${UNTIL_OPUS} | color=#888888"
fi

echo "---"
echo "Refresh | refresh=true"
