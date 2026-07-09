#!/usr/bin/env bash
# sudoers-setup.sh — install or remove the NOPASSWD sudoers rule that lets the
# disable-sleep xbar plugin run `pmset -b disablesleep 0|1` without a password.
#
# Without this rule, every menubar toggle falls back to an osascript admin
# prompt — and osascript has no cross-process auth cache, so the dialog pops
# on every single click.
#
# Usage:
#   ./sudoers-setup.sh             # install (default)
#   ./sudoers-setup.sh install
#   ./sudoers-setup.sh uninstall
#
# Re-execs through sudo from a terminal, or `osascript ... with administrator
# privileges` from a non-TTY context (xbar's menu, curl|bash).
#
# Idempotent: if /etc/sudoers.d/xbar-disable-sleep already exists, `install`
# exits 0 without escalating, so re-running setup.sh doesn't re-prompt.

set -u

SUDOERS_FILE="/etc/sudoers.d/xbar-disable-sleep"

# Resolve own absolute path so re-exec via sudo / osascript hits this same file.
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
SELF="$SELF_DIR/$(basename "${BASH_SOURCE[0]}")"

action="${1:-install}"
case "$action" in
  install)   reason="xbar disable-sleep: install passwordless sudoers rule so future toggles don't prompt." ;;
  uninstall) reason="Remove the sudoers rule for xbar's sleep toggle." ;;
  *) echo "Usage: $0 [install|uninstall]" >&2; exit 2 ;;
esac

# Idempotency: if the rule is already in place, don't escalate. `[[ -f ]]`
# works without root, so this short-circuits before sudo / osascript.
if [[ "$action" == "install" && -f "$SUDOERS_FILE" ]]; then
  echo "sudoers rule already present: $SUDOERS_FILE"
  echo "(remove with: sudo rm \"$SUDOERS_FILE\")"
  exit 0
fi
if [[ "$action" == "uninstall" && ! -f "$SUDOERS_FILE" ]]; then
  echo "Already absent: $SUDOERS_FILE"
  exit 0
fi

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
