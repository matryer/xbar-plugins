#!/usr/bin/env bash

# <xbar.title>Disable Sleep Toggle</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Kipras Melnikovas</xbar.author>
# <xbar.author.github>kiprasmel</xbar.author.github>
# <xbar.desc>One-click toggle for `sudo pmset -b disablesleep` with a bed / crossed-out-bed icon. Keeps the Mac awake on battery while agents work.</xbar.desc>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/kiprasmel/xbar-plugins/tree/main/System/disable-sleep</xbar.abouturl>

set -u

# Resolve absolute path of $0 (following symlinks). Portable across macOS
# versions — does not rely on /usr/bin/realpath, which was missing pre-Big Sur.
resolve_path() {
  local target="$1"
  while [[ -L "$target" ]]; do
    local link
    link="$(readlink "$target")"
    if [[ "$link" = /* ]]; then
      target="$link"
    else
      target="$(cd "$(dirname "$target")" && pwd -P)/$link"
    fi
  done
  echo "$(cd "$(dirname "$target")" && pwd -P)/$(basename "$target")"
}

SELF="$(resolve_path "$0")"
DIR="$(dirname "$SELF")"

# Toggle: try passwordless via sudo -n (handy if you have a NOPASSWD rule
# for `pmset -b disablesleep 0|1`), fall back to a GUI password prompt via
# osascript with a reason string so it's not a mystery dialog.
toggle_pmset() {
  local val="$1"
  local action; [[ "$val" == "1" ]] && action="disable" || action="allow"
  local reason="Toggle battery sleep (pmset -b disablesleep $val → $action)."
  sudo -n /usr/bin/pmset -b disablesleep "$val" 2>/dev/null \
    || osascript -e "do shell script \"/usr/bin/pmset -b disablesleep $val\" with prompt \"$reason\" with administrator privileges" >/dev/null
}

case "${1:-}" in
  deny_sleep)  toggle_pmset 1; exit ;;
  allow_sleep) toggle_pmset 0; exit ;;
esac

sleep_disabled="$(pmset -g | awk '$1~/SleepDisabled/ {print $2}')"

if [[ "$sleep_disabled" == "1" ]]; then
  icon_b64="$(base64 < "$DIR/bed-no.png" | tr -d '\n')"
  toggle_label="Allow sleep"
  toggle_param="allow_sleep"
  status_text="Battery sleep: DENIED"
else
  icon_b64="$(base64 < "$DIR/bed.png" | tr -d '\n')"
  toggle_label="Disable sleep"
  toggle_param="deny_sleep"
  status_text="Battery sleep: allowed"
fi

echo "| templateImage=$icon_b64"
echo "---"
echo "$toggle_label | bash='$SELF' param1=$toggle_param terminal=false refresh=true size=14"
echo "---"
echo "$status_text | disabled=true"
echo "Refresh | refresh=true"
