#!/usr/bin/env bash
# setup.sh — install/uninstall the sudoers rule that makes the disable-sleep
# xbar plugin's toggle passwordless.
#
# Adds /etc/sudoers.d/xbar-disable-sleep with an exact, two-command allow-list:
#   <user> ALL=(root) NOPASSWD: /usr/bin/pmset -b disablesleep 0,
#                               /usr/bin/pmset -b disablesleep 1
#
# Usage:
#   ./setup.sh install [username]   # install rule (defaults to current user)
#   ./setup.sh uninstall            # remove rule
#   ./setup.sh status               # exit 0 if installed, 1 if missing
#
# This script self-escalates: from a terminal it re-execs through `sudo`; from
# xbar's menu (no TTY) it prompts via `osascript ... with administrator
# privileges`. Either way, the sudoers content is validated with `visudo -cf`
# on a temp file BEFORE it's installed at the destination.

set -u

# Resolve absolute path of $0, following symlinks. Portable across macOS
# versions (pre-Big Sur didn't ship /usr/bin/realpath).
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
SUDOERS_FILE="/etc/sudoers.d/xbar-disable-sleep"

err()  { printf '%s\n' "$*" >&2; }
info() { printf '%s\n' "$*"; }

# Re-exec self as root, preserving the argv it was originally called with.
# Uses plain `sudo` from a terminal, falls back to a GUI password prompt via
# osascript when there's no controlling TTY (e.g. invoked from an xbar menu
# item).
escalate() {
  if [[ -t 0 ]]; then
    exec sudo "$SELF" "$@"
  fi

  local sh_cmd
  sh_cmd="$(printf '%q ' "$SELF" "$@")"
  local as_str="${sh_cmd//\\/\\\\}"
  as_str="${as_str//\"/\\\"}"
  osascript -e "do shell script \"$as_str\" with administrator privileges" >/dev/null
  exit $?
}

cmd_install() {
  local target_user="${1:-${SUDO_USER:-$USER}}"

  if [[ -z "$target_user" || "$target_user" == "root" ]]; then
    err "Refusing to install rule for empty/root user; pass a username explicitly."
    exit 2
  fi

  if [[ "$EUID" -ne 0 ]]; then
    escalate install "$target_user"
    exit 1  # unreachable; escalate either execs or exits
  fi

  local tmp
  tmp="$(mktemp)"
  cat > "$tmp" <<EOF
# Installed by xbar plugin: disable-sleep.10s.sh
# See: https://github.com/kiprasmel/xbar-plugins/tree/main/System/disable-sleep
$target_user ALL=(root) NOPASSWD: /usr/bin/pmset -b disablesleep 0, /usr/bin/pmset -b disablesleep 1
EOF

  if ! /usr/sbin/visudo -cf "$tmp" >/dev/null 2>&1; then
    rm -f "$tmp"
    err "visudo validation failed; nothing installed."
    exit 1
  fi

  /usr/bin/install -m 0440 -o root -g wheel "$tmp" "$SUDOERS_FILE"
  rm -f "$tmp"
  info "Installed: $SUDOERS_FILE (user=$target_user)"
}

cmd_uninstall() {
  if [[ "$EUID" -ne 0 ]]; then
    escalate uninstall
    exit 1
  fi
  if [[ -f "$SUDOERS_FILE" ]]; then
    rm -f "$SUDOERS_FILE"
    info "Removed: $SUDOERS_FILE"
  else
    info "Already absent: $SUDOERS_FILE"
  fi
}

cmd_status() {
  if [[ -f "$SUDOERS_FILE" ]]; then
    info "installed: $SUDOERS_FILE"
    exit 0
  fi
  info "missing: $SUDOERS_FILE"
  exit 1
}

usage() {
  cat <<USAGE
Usage:
  $0 install [username]   # install sudoers rule (defaults to current user)
  $0 uninstall            # remove sudoers rule
  $0 status               # check whether rule is installed
USAGE
}

case "${1:-help}" in
  install)             shift; cmd_install   "$@" ;;
  uninstall)           shift; cmd_uninstall "$@" ;;
  status)              shift; cmd_status    "$@" ;;
  help|-h|--help)      usage ;;
  *) err "Unknown subcommand: $1"; usage >&2; exit 2 ;;
esac
