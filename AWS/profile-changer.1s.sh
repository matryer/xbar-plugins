#!/usr/bin/env bash
# <xbar.title>AWS Profile Switcher</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>ghkdqhrbals</xbar.author>
# <xbar.desc>Switches AWS default profile using nested menus.</xbar.desc>
# <xbar.abouturl>https://github.com/ghkdqhrbals/xbar-plugins-guideline/blob/main/README.md</xbar.abouturl>
CREDENTIALS_FILE="$HOME/.aws/credentials"
STATE_FILE="$HOME/.aws_profile_state"
ERROR_FILE="$HOME/.aws_profile_error"

detect_current_profile() {
  local default_key
  default_key=$(awk '/\[default\]/{f=1} f && /aws_access_key_id/ {print $3; exit}' "$CREDENTIALS_FILE")

  if [ -z "$default_key" ]; then
    echo "(unknown)"
    return
  fi

  awk -v def_key="$default_key" '
    BEGIN { profile = ""; matched = 0 }
    /^\[.*\]$/ {
      gsub(/\[|\]/, "", $0)
      profile = $0
    }
    $1 == "aws_access_key_id" && $3 == def_key && profile != "default" {
      print profile
      matched = 1
      exit
    }
    END {
      if (matched == 0) print "(unknown)"
    }
  ' "$CREDENTIALS_FILE"
}

# inline aws profile change
is_valid() {
  [[ -f "$CREDENTIALS_FILE" ]] || return 1
  grep -qx "^\[$1\]$" "$CREDENTIALS_FILE"
}

get_aws_region() {
  local profile="$1"
  [[ -f "$HOME/.aws/config" ]] || return 1

  # config 파일은 [profile profilename] 형식임!
  sed -n "/^\[profile $profile\]/,/^\[/p" "$HOME/.aws/config" \
    | grep "region" | awk -F'=' '{print $2}' | tr -d ' '
}

get_aws_credentials() {
  local profile="$1"
  is_valid "$profile" || return 1

  local access_key secret_key
  access_key=$(sed -n "/^\[$profile\]/,/^\[/p" "$CREDENTIALS_FILE" | grep "aws_access_key_id" | awk -F'=' '{print $2}' | tr -d ' ')
  secret_key=$(sed -n "/^\[$profile\]/,/^\[/p" "$CREDENTIALS_FILE" | grep "aws_secret_access_key" | awk -F'=' '{print $2}' | tr -d ' ')

  if [[ -z "$access_key" || -z "$secret_key" ]]; then
    return 1
  fi

  echo "$access_key $secret_key"
}

update_aws_profile() {
  local profile="$1"
  local creds
  creds=$(get_aws_credentials "$profile") || return 1

  local access_key=$(echo "$creds" | awk '{print $1}')
  local secret_key=$(echo "$creds" | awk '{print $2}')

  aws configure set aws_access_key_id "$access_key" --profile default
  aws configure set aws_secret_access_key "$secret_key" --profile default

  local region
  region=$(get_aws_region "$profile")

  if [[ -n "$region" ]]; then
    aws configure set region "$region" --profile default
  fi

  return 0
}

# 실행: 선택된 프로파일로 바꾸기
if [[ "$1" != "" ]]; then
  echo "$1:loading" > "$STATE_FILE"
  sleep 0.5
  if update_aws_profile "$1"; then
    echo "$1:done" > "$STATE_FILE"
    rm -f "$ERROR_FILE"
  else
    echo "$1:fail" > "$STATE_FILE"
    echo "❌ Failed to switch to [$1]" > "$ERROR_FILE"
  fi
  exit 0
fi

# 출력용 정보
current_profile=$(detect_current_profile)
if [[ -f "$STATE_FILE" ]]; then
  state=$(cat "$STATE_FILE")
  current_status="${state##*:}"
else
  current_status="done"
fi

# 아이콘
case "$current_status" in
  loading) icon="Loading..." ;;
  done) icon="☁️ $current_profile | font=Monaco size=12 color=#ffffff" ;;
  fail) icon="❌" ;;
  *) icon="⚪" ;;
esac

echo "$icon"

echo "---"
if [[ "$current_status" == "fail" && -f "$ERROR_FILE" ]]; then
  echo "$(cat "$ERROR_FILE") | color=#ff4444 font=Monaco size=11"
  echo "---"
fi
echo "---"
echo "aws profile"
# 메뉴 출력
echo "$current_profile"
aws_profiles=$(awk '/\[[^\]]+\]/ { gsub(/\[|\]/, "", $0); if ($0 != "default") print $0 }' "$CREDENTIALS_FILE")

for profile in $aws_profiles; do
  if [[ "$current_profile" == "$profile" ]]; then
    echo "-- ▶ $profile (active) | bash='$0' param1=$profile terminal=false refresh=true font=Monaco size=12 color=#00aa00"
  else
    echo "-- $profile | bash='$0' param1=$profile terminal=false refresh=true font=Monaco size=12 color=#000000"
  fi
done