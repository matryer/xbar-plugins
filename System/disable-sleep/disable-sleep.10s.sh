#!/usr/bin/env bash

# <xbar.title>Disable Sleep Toggle</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kipras Melnikovas</xbar.author>
# <xbar.author.github>kiprasmel</xbar.author.github>
# <xbar.desc>One-click toggle for `sudo pmset -b disablesleep` with a bed / crossed-out-bed icon. Keeps the Mac awake on battery while agents work.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/bed.png</xbar.image>
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

# Surface failures from a menubar click — stderr disappears into xbar's void,
# so use an osascript dialog instead.
show_error() {
  local msg="$1"
  local esc="${msg//\\/\\\\}"; esc="${esc//\"/\\\"}"
  osascript \
    -e "display dialog \"$esc\" with title \"xbar disable-sleep\" buttons {\"OK\"} default button 1 with icon caution" \
    >/dev/null 2>&1 || true
}

# Toggle: try passwordless via sudo -n (the NOPASSWD rule installed by
# sudoers-setup.sh). If that fails, self-heal by running sudoers-setup.sh —
# it prompts once via osascript admin, installs the rule, then we retry
# pmset passwordlessly. One prompt total, and future clicks need none.
toggle_pmset() {
  local val="$1"

  if sudo -n /usr/bin/pmset -b disablesleep "$val" 2>/dev/null; then
    return 0
  fi

  local setup="$DIR/sudoers-setup.sh"
  if [[ ! -x "$setup" ]]; then
    show_error "sudoers-setup.sh not found at $setup; cannot self-heal."
    return 1
  fi

  if ! "$setup" install >/dev/null 2>&1; then
    show_error "Couldn't install the sudoers rule. Re-run manually:
$setup install"
    return 1
  fi

  if ! sudo -n /usr/bin/pmset -b disablesleep "$val" 2>/dev/null; then
    show_error "Installed sudoers rule but pmset still failed. Check /etc/sudoers.d/xbar-disable-sleep."
    return 1
  fi
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
