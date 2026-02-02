#!/usr/bin/env bash
#<xbar.title>Poe Balance</xbar.title>
#<xbar.version>1.0</xbar.version>
#<xbar.author>Rodrigo Nemmen da Silva</xbar.author>
#<xbar.desc>Display remaining Poe API credits</xbar.desc>
#<xbar.image>https://github.com/rsnemmen/poe-balance/blob/780b20c79f3433f1908353888a6fa59217f3b8f9/images/cover.png</xbar.image>
#<xbar.dependencies>curl,bc</xbar.dependencies>

# User variables
# ================
# For detailed instructions, visit https://github.com/rsnemmen/poe-balance
#
#<xbar.var>number(VAR_STARTING_DATE="0"): Billing period starting date (1-31). Set to 0 to disable.</xbar.var>
#<xbar.var>boolean(VAR_PERCENT="true"): Display remaining balance as percentage?.</xbar.var>

STARTING_DATE=$VAR_STARTING_DATE
PERCENT=$VAR_PERCENT
INITIAL_BALANCE=1000000

POE_ICON="iVBORw0KGgoAAAANSUhEUgAAADIAAAAqCAMAAADGZPh1AAAACVBMVEUAAAD///////9zeKVjAAAAA3RSTlP//wDXyg1BAAAACXBIWXMAAC4jAAAuIwF4pT92AAAE9WlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPD94cGFja2V0IGJlZ2luPSLvu78iIGlkPSJXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQiPz4gPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iQWRvYmUgWE1QIENvcmUgMTAuMC1jMDAwIDc5LmQwNGNjMTY5OCwgMjAyNS8wNy8wMi0xMjoxODoxMyAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iIHhtbG5zOnBob3Rvc2hvcD0iaHR0cDovL25zLmFkb2JlLmNvbS9waG90b3Nob3AvMS4wLyIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHhtbG5zOnN0RXZ0PSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3VyY2VFdmVudCMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIDI3LjMgKE1hY2ludG9zaCkiIHhtcDpDcmVhdGVEYXRlPSIyMDI2LTAyLTAxVDIwOjEzOjI0LTA4OjAwIiB4bXA6TW9kaWZ5RGF0ZT0iMjAyNi0wMi0wMVQyMTowNTo0Ni0wODowMCIgeG1wOk1ldGFkYXRhRGF0ZT0iMjAyNi0wMi0wMVQyMTowNTo0Ni0wODowMCIgZGM6Zm9ybWF0PSJpbWFnZS9wbmciIHBob3Rvc2hvcDpDb2xvck1vZGU9IjIiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6ZDA3MjI4ODItYWE3NC00MThjLThhZjYtMmQ0MzJmYjhhMDVkIiB4bXBNTTpEb2N1bWVudElEPSJ4bXAuZGlkOmQwNzIyODgyLWFhNzQtNDE4Yy04YWY2LTJkNDMyZmI4YTA1ZCIgeG1wTU06T3JpZ2luYWxEb2N1bWVudElEPSJ4bXAuZGlkOmQwNzIyODgyLWFhNzQtNDE4Yy04YWY2LTJkNDMyZmI4YTA1ZCI+IDx4bXBNTTpIaXN0b3J5PiA8cmRmOlNlcT4gPHJkZjpsaSBzdEV2dDphY3Rpb249ImNyZWF0ZWQiIHN0RXZ0Omluc3RhbmNlSUQ9InhtcC5paWQ6ZDA3MjI4ODItYWE3NC00MThjLThhZjYtMmQ0MzJmYjhhMDVkIiBzdEV2dDp3aGVuPSIyMDI2LTAyLTAxVDIwOjEzOjI0LTA4OjAwIiBzdEV2dDpzb2Z0d2FyZUFnZW50PSJBZG9iZSBQaG90b3Nob3AgMjcuMyAoTWFjaW50b3NoKSIvPiA8L3JkZjpTZXE+IDwveG1wTU06SGlzdG9yeT4gPC9yZGY6RGVzY3JpcHRpb24+IDwvcmRmOlJERj4gPC94OnhtcG1ldGE+IDw/eHBhY2tldCBlbmQ9InIiPz7iX8W6AAAA40lEQVRIib1USRIEIQjTFP9/MjXVpdi4sNiHyWlsSAgMWlE6uFiAcWSTsYYQM9YgyjXwmcJ+1hSukE92vSVa0SR8g1NKnQ26xnpOzU1Bc8g2YBlGuQb28nw4SAL/+99Pg9mZiCXGUyA3UWwnHA8K5CgY1oD88suOyfJ7oxthfilRlZcBll7gc6BFEcgrb5JOF4wun6QIgbOU+RmiU+xMk3YoZerJHreLjkquQSQZakexanj6DWIMz83xHoPXAMmPwYjWrSjRiCGp5MucUHWZZi7a1HnIcSObidT7uSXEF0j3ksQPw1c2eLCW14gAAAAASUVORK5CYII="

# === Grabs API key === 
# (inspired by Dev/openai.30m.sh plugin)

# Method 1: Environment variable (works in terminal)
if [ -n "$POE_API_KEY" ]; then
    API_KEY="$POE_API_KEY"
fi

# Method 2: from Z shell config file (works for GUI apps like SwiftBar)
if [ -z "$API_KEY" ] && [ -f "$HOME/.zshrc" ]; then
    API_KEY=$(grep '^export POE_API_KEY=' "$HOME/.zshrc" | cut -d'=' -f2 | tr -d '"' | tr -d "'")
fi

# Method 3: from bash config file 
if [ -z "$API_KEY" ] && [ -f "$HOME/.bashrc" ]; then
    API_KEY=$(grep '^export POE_API_KEY=' "$HOME/.bashrc" | cut -d'=' -f2 | tr -d '"' | tr -d "'")
fi

# Prefer SwiftBar-provided API_KEY; fallback to POE_API_KEY from environment
API_KEY="${API_KEY:-${POE_API_KEY:-}}"

if [ -z "$API_KEY" ]; then
  echo "⚠️ No API Key"
  echo "Missing API key. Set API_KEY via export POE_API_KEY in .bashrc or .zshrc." >&2
  exit 1
fi

# === Fetch balance ===
response="$(curl -s -w "\n%{http_code}" \
  -H "Authorization: Bearer ${API_KEY}" \
  -H "Accept: application/json" \
  "https://api.poe.com/usage/current_balance")"

http_code="$(printf '%s\n' "$response" | tail -n 1)"
body="$(printf '%s\n' "$response" | sed '$d')"

if [ "$http_code" = "401" ]; then
  echo "⚠️ Invalid Key"
  exit 1
elif [ "$http_code" -lt 200 ] || [ "$http_code" -ge 300 ]; then
  echo "⚠️ POE API Error ($http_code)"
  echo "Response body: $body" >&2
  exit 1
fi

# Extract balance from JSON (kept simple; assumes integer field)
balance="$(printf '%s\n' "$body" | sed -n 's/.*"current_point_balance"[[:space:]]*:[[:space:]]*\([0-9][0-9]*\).*/\1/p')"

if [ -z "$balance" ]; then
  echo "⚠️ Parse Error"
  echo "Could not parse current_point_balance from: $body" >&2
  exit 1
fi

# === Format number ===
# (e.g., 693000 -> 693k)
format_number() {
  local n="$1"
  if [ "$n" -lt 1000 ]; then
    echo "$n"
  elif [ "$n" -lt 1000000 ]; then
    echo "$((n / 1000))k"
  elif [ "$n" -lt 1000000000 ]; then
    echo "$(echo "scale=1; $n / 1000000" | bc | sed 's/\.0$//')M"
  else
    echo "$(echo "scale=1; $n / 1000000000" | bc | sed 's/\.0$//')B"
  fi
}

# Round number to integer
round() {
  printf "%.0f" "$(echo "scale=10; $1" | bc)"
}

formatted="$(format_number "$balance")"

# if STARTING_DATE=0 or the variable was not defined, then the code displays only the 
# remaining balance
if [ "$PERCENT" = "true" ]; then
  # Calculate percentage
  pct=$(round "$balance / 10000")
  
  if [ -n "$STARTING_DATE" ] && [ $STARTING_DATE -gt 0 ]; then
    # Validate STARTING_DATE (1-31)
    if [ "$STARTING_DATE" -gt 31 ]; then
      echo "⚠️ Invalid STARTING_DATE" >&2
      exit 1
    fi

    # === Compute DAYS passed since STARTING_DATE ===
    # Compute DAYS passed since STARTING_DATE as a fractional number
    TODAY=$((10#$(date +%d)))   # force base-10 (avoids issues with leading zeros)
    NOW_S=$(date +%s)

    if [ "$STARTING_DATE" -le "$TODAY" ]; then
      # start date is in the current month at 00:00:00
      START_S=$(date -j -v"${STARTING_DATE}"d -v0H -v0M -v0S +%s)
    else
      # start date is in the previous month at 00:00:00
      START_S=$(date -j -v-1m -v"${STARTING_DATE}"d -v0H -v0M -v0S +%s)
    fi

    DAYS=$(echo "scale=4; ($NOW_S - $START_S) / 86400" | bc)  # e.g. 4.3333

    # Compute estimated spent: 1M credits/day * DAYS
    DAILY_CREDITS=32895 # 1E6/30.4, assumes equal usage every day
    ESTIMATED_SPENT=$(round "$INITIAL_BALANCE - ($DAYS * $DAILY_CREDITS)")

    # Calculate estimated percentage
    est_pct=$(round "$ESTIMATED_SPENT / 10000")
    
    # SwiftBar output (header)
    echo "${pct}% (Est.: ${est_pct}%) | templateImage=$POE_ICON"
  else
    echo "${pct}% | templateImage=$POE_ICON"
  fi
else
  if [ -n "$STARTING_DATE" ] && [ $STARTING_DATE -gt 0 ]; then
    # Validate STARTING_DATE (1-31)
    if [ "$STARTING_DATE" -gt 31 ]; then
      echo "⚠️ Invalid STARTING_DATE" >&2
      exit 1
    fi

    # Compute DAYS passed since STARTING_DATE (exclusive)
    TODAY=$(date +%d)
    if [ "$STARTING_DATE" -le "$TODAY" ]; then
      DAYS=$((TODAY - STARTING_DATE))
    else
      DAYS=$TODAY
    fi

    # Compute estimated spent: 1M credits/day * DAYS
    DAILY_CREDITS=32895 # 1E6/30.4, assumes equal usage every day
    ESTIMATED_SPENT=$((INITIAL_BALANCE-DAYS * DAILY_CREDITS))

    # === SwiftBar output ===
    echo "$formatted (Est.: $(format_number "$ESTIMATED_SPENT")) | templateImage=$POE_ICON"
  else
    echo "$formatted | templateImage=$POE_ICON"
  fi
fi