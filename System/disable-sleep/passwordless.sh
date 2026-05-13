#!/usr/bin/env bash
# passwordless.sh — install or remove the NOPASSWD sudoers rule that lets the
# disable-sleep xbar plugin run `pmset -b disablesleep 0|1` without a password.
#
# Usage:
#   ./passwordless.sh             # install (default)
#   ./passwordless.sh install
#   ./passwordless.sh uninstall
#
# Re-execs through sudo from a terminal, or `osascript ... with administrator
# privileges` from a non-TTY context (xbar's menu).

set -u

SUDOERS_FILE="/etc/sudoers.d/xbar-disable-sleep"

# Resolve own absolute path so re-exec via sudo / osascript hits this same file.
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SELF="$SELF_DIR/$(basename "${BASH_SOURCE[0]}")"

action="${1:-install}"
case "$action" in
  install)   reason="Install sudoers rule so xbar can toggle sleep without a password." ;;
  uninstall) reason="Remove the sudoers rule for xbar's sleep toggle." ;;
  *) echo "Usage: $0 [install|uninstall]" >&2; exit 2 ;;
esac

if [[ "$EUID" -ne 0 ]]; then
  if [[ -t 0 ]]; then
    echo
    echo "→ $reason"
    exec sudo "$SELF" "$action"
  fi
  esc_self="${SELF//\\/\\\\}"; esc_self="${esc_self//\"/\\\"}"
  prompt="${reason//\\/\\\\}"; prompt="${prompt//\"/\\\"}"
  osascript -e "do shell script \"$esc_self $action\" with prompt \"$prompt\" with administrator privileges" >/dev/null
  exit $?
fi

target_user="${SUDO_USER:-$USER}"
if [[ -z "$target_user" || "$target_user" == "root" ]]; then
  echo "Refusing to manage rule for empty/root user." >&2
  exit 2
fi

case "$action" in
  install)
    tmp="$(mktemp)"
    cat > "$tmp" <<EOF
# Installed by xbar plugin: disable-sleep.10s.sh
# See: https://github.com/kiprasmel/xbar-plugins/tree/main/System/disable-sleep
$target_user ALL=(root) NOPASSWD: /usr/bin/pmset -b disablesleep 0, /usr/bin/pmset -b disablesleep 1
EOF
    if ! /usr/sbin/visudo -cf "$tmp" >/dev/null 2>&1; then
      rm -f "$tmp"
      echo "visudo validation failed; sudoers not installed." >&2
      exit 1
    fi
    /usr/bin/install -m 0440 -o root -g wheel "$tmp" "$SUDOERS_FILE"
    rm -f "$tmp"
    echo "Installed: $SUDOERS_FILE (user=$target_user)"
    ;;
  uninstall)
    if [[ -f "$SUDOERS_FILE" ]]; then
      rm -f "$SUDOERS_FILE"
      echo "Removed: $SUDOERS_FILE"
    else
      echo "Already absent: $SUDOERS_FILE"
    fi
    ;;
esac
