#!/usr/bin/env bash

# <xbar.title>Disable Sleep Toggle</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Kipras Melnikovas</xbar.author>
# <xbar.author.github>kiprasmel</xbar.author.github>
# <xbar.desc>One-click toggle for `sudo pmset -b disablesleep` with a bed / crossed-out-bed icon. Pair with setup.sh for passwordless toggling so the Mac stays awake on battery while agents work.</xbar.desc>
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
HELPER="$DIR/setup.sh"
SUDOERS_FILE="/etc/sudoers.d/xbar-disable-sleep"

# Toggle: try passwordless via sudo -n (requires setup.sh install), fall back
# to GUI password prompt via osascript so the toggle still works pre-setup.
toggle_pmset() {
  local val="$1"
  sudo -n /usr/bin/pmset -b disablesleep "$val" 2>/dev/null \
    || osascript -e "do shell script \"/usr/bin/pmset -b disablesleep $val\" with administrator privileges" >/dev/null
}

case "${1:-}" in
  deny_sleep)  toggle_pmset 1; exit ;;
  allow_sleep) toggle_pmset 0; exit ;;
esac

sleep_disabled="$(pmset -g | awk '$1~/SleepDisabled/ {print $2}')"
sudoers_installed=0
[[ -f "$SUDOERS_FILE" ]] && sudoers_installed=1

if [[ "$sleep_disabled" == "1" ]]; then
  icon_b64="$(base64 < "$DIR/bed-no.png" | tr -d '\n')"
  toggle_label="Allow sleep on battery"
  toggle_param="allow_sleep"
  status_text="Battery sleep: DENIED"
else
  icon_b64="$(base64 < "$DIR/bed.png" | tr -d '\n')"
  toggle_label="Disable sleep on battery"
  toggle_param="deny_sleep"
  status_text="Battery sleep: allowed"
fi

echo "| templateImage=$icon_b64"
echo "---"
# The single prominent toggle button — sized up so it visually dominates the dropdown.
echo "$toggle_label | bash='$SELF' param1=$toggle_param terminal=false refresh=true size=14"
echo "---"
echo "$status_text | disabled=true"
if [[ ! -x "$HELPER" ]]; then
  echo "Passwordless toggle: setup.sh missing | disabled=true"
elif [[ "$sudoers_installed" == "1" ]]; then
  echo "Passwordless toggle: enabled | disabled=true"
  echo "Uninstall passwordless toggle | bash='$HELPER' param1=uninstall terminal=false refresh=true"
else
  echo "Passwordless toggle: not installed | disabled=true"
  echo "Install passwordless toggle… | bash='$HELPER' param1=install terminal=false refresh=true"
fi
echo "Refresh | refresh=true"
