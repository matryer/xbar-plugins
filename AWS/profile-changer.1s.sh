#!/usr/bin/env bash
# <xbar.title>AWS Profile Switcher</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>ghkdqhrbals</xbar.author>
# <xbar.desc>Switches AWS default profile using nested menus.</xbar.desc>
# <xbar.abouturl>https://github.com/ghkdqhrbals/xbar-plugins-guideline/blob/main/README.md</xbar.abouturl>

CREDENTIALS_FILE="$HOME/.aws/credentials"
STATE_FILE="$HOME/.aws_profile_state"
ERROR_FILE="$HOME/.aws_profile_error"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
UPDATER="$SCRIPT_DIR/profile-changer/update.sh"

if [[ ! -f "$UPDATER" || $(find "$UPDATER" -mtime +0) ]]; then

  mkdir -p "$SCRIPT_DIR/profile-changer"
  cat > "$UPDATER" <<'EOF'

#!/usr/bin/env bash

PROFILE=$1
CREDENTIALS_FILE="$HOME/.aws/credentials"
STATE_FILE="$HOME/.aws_profile_state"
ERROR_FILE="$HOME/.aws_profile_error"
LOG_FILE="/tmp/aws-profile.log"

log() {
  echo "[$(date +'%F %T')] $*" >> "$LOG_FILE"
}

update_credentials_file() {
  creds_block=$(awk "/^\[$PROFILE\]/ {flag=1; next} /^\[.*\]/ {flag=0} flag" "$CREDENTIALS_FILE")

  if [[ -z "$creds_block" ]]; then
    echo "$PROFILE:fail" > "$STATE_FILE"
    echo "❌ Failed to find profile [$PROFILE]" > "$ERROR_FILE"
    log "Profile [$PROFILE] not found in credentials file"
    exit 1
  fi

  tmp_file=$(mktemp)

  awk '
    BEGIN {in_default=0}
    /^\[default\]/ {in_default=1; next}
    /^\[.*\]/ {
      if (in_default) {in_default=0}
    }
    !in_default
  ' "$CREDENTIALS_FILE" > "$tmp_file"

  {
    echo "[default]"
    echo "$creds_block"
  } >> "$tmp_file"

  mv "$tmp_file" "$CREDENTIALS_FILE"
}

update_config_file() {
  region=$(awk "/^\[profile $PROFILE\]/ {flag=1; next} /^\[.*\]/ {flag=0} flag && /region/" "$CONFIG_FILE" | awk -F= '{print $2}' | tr -d ' ')

  if [[ -z "$region" ]]; then
    log "No region found for [$PROFILE] in config. Skipping config update."
    return
  fi

  tmp_cfg=$(mktemp)

  awk '
    BEGIN {in_default=0}
    /^\[default\]/ {in_default=1; next}
    /^\[.*\]/ {
      if (in_default) {in_default=0}
    }
    !in_default
  ' "$CONFIG_FILE" > "$tmp_cfg"

  {
    echo "[default]"
    echo "region = $region"
  } >> "$tmp_cfg"

  mv "$tmp_cfg" "$CONFIG_FILE"
  log "Updated region in config [default] to: $region"
}

echo "$PROFILE:loading" > "$STATE_FILE"
log "Switching to [$PROFILE]"

update_credentials_file
update_config_file

echo "$PROFILE:done" > "$STATE_FILE"
rm -f "$ERROR_FILE"
log "Successfully updated [default] to [$PROFILE]"

if [[ "$TERM_PROGRAM" == "Apple_Terminal" ]]; then
  sleep 0.2
  osascript -e 'tell application "Terminal" to close (every window whose visible is true and frontmost is true)' &
fi

exit 0
EOF

chmod +x "$UPDATER"

fi


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


current_profile=$(detect_current_profile)
if [[ -f "$STATE_FILE" ]]; then
  state=$(cat "$STATE_FILE")
  current_status="${state##*:}"
else
  current_status="done"
fi

case "$current_status" in
  loading) icon="🔄 Switching..." ;;
  done) icon="☁️ $current_profile | font=Monaco size=12 color=#ffffff" ;;
  fail) icon="❌ Failed to switch profile" ;;
  *) icon="⚪ Unknown" ;;
esac

echo "$icon"
echo "---"

if [[ "$current_status" == "fail" && -f "$ERROR_FILE" ]]; then
  echo "$(cat "$ERROR_FILE") | color=#ff4444 font=Monaco size=11"
  echo "---"
fi

echo "aws profile"
echo "$current_profile"
echo "---"

aws_profiles=$(awk '/\[[^\]]+\]/ { gsub(/\[|\]/, "", $0); if ($0 != "default") print $0 }' "$CREDENTIALS_FILE")

for profile in $aws_profiles; do
  if [[ "$current_profile" == "$profile" ]]; then
    echo "-- ▶ $profile (active) | bash='$UPDATER' param1=$profile terminal=false refresh=true font=Monaco size=12 color=#00aa00"
  else
    echo "-- $profile | bash='$UPDATER' param1=$profile terminal=false refresh=true font=Monaco size=12 color=#000000"
  fi
done