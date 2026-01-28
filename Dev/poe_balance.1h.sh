#!/usr/bin/env bash
#<xbar.title>Poe Balance</xbar.title>
#<xbar.version>1.0</xbar.version>
#<xbar.author>Rodrigo Nemmen da Silva</xbar.author>
#<xbar.desc>Display remaining Poe API credits</xbar.desc>
#<xbar.dependencies>curl,bc</xbar.dependencies>

# Grabs API key (inspired by Dev/openai.30m.sh plugin)
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
  echo "Missing API key. Set API_KEY via <swiftbar.environment> or export POE_API_KEY if running in terminal." >&2
  exit 1
fi

# Fetch balance
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

# Format number (e.g., 693000 -> 693k)
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

formatted="$(format_number "$balance")"

# SwiftBar output (header)
echo "Poe: $formatted"