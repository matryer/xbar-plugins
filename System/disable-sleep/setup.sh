#!/usr/bin/env bash
# setup.sh — local installer for the disable-sleep xbar plugin.
#
# Does the whole local hook-up:
#   1. Symlink this directory into xbar's plugins folder so xbar discovers it.
#   2. Install a NOPASSWD sudoers rule (/etc/sudoers.d/xbar-disable-sleep) so
#      `pmset -b disablesleep 0|1` runs without a password prompt.
#
# Either step is skipped if it's already in the desired state, so re-running
# `setup.sh` is safe.
#
# Usage:
#   ./setup.sh [install [username]]  # symlinks + sudoers (default; current user)
#   ./setup.sh uninstall             # remove both symlinks and sudoers
#   ./setup.sh status                # show what's installed where
#
# Env:
#   XBAR_PLUGINS_DIR=<path>   override xbar plugins folder (else
#                             ~/Library/Application Support/xbar/plugins)
#
# Self-escalates: terminal invocations re-exec via `sudo` for the sudoers
# step; non-TTY invocations (from xbar's menu) use the system password
# prompt via `osascript ... with administrator privileges`. Symlinks are
# created BEFORE escalation, in user-space, so admin powers are limited to
# touching /etc/sudoers.d only.

set -u

# Resolve absolute path of $0, following symlinks. Portable across macOS
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
SUDOERS_FILE="/etc/sudoers.d/xbar-disable-sleep"

err()  { printf '%s\n' "$*" >&2; }
info() { printf '%s\n' "$*"; }

# Look up a user's xbar plugins folder. Honors $XBAR_PLUGINS_DIR if set,
# otherwise resolves $user's home via dscl (works even when running under
# `sudo`, where $HOME would otherwise be /var/root).
plugins_dir_for() {
  local user="$1"
  if [[ -n "${XBAR_PLUGINS_DIR:-}" ]]; then
    echo "$XBAR_PLUGINS_DIR"
    return
  fi
  local home
  if [[ "$user" == "$USER" && "$EUID" -ne 0 ]]; then
    home="$HOME"
  else
    home="$(/usr/bin/dscl . -read "/Users/$user" NFSHomeDirectory 2>/dev/null | awk '{print $2}')"
  fi
  if [[ -z "$home" ]]; then
    err "Could not resolve home directory for user '$user'."
    exit 3
  fi
  echo "$home/Library/Application Support/xbar/plugins"
}

# Ensure xbar can discover the plugin by symlinking this directory (and its
# main script) into $plugins. Idempotent. Refuses to clobber an existing
# real directory at the target — symlinks are replaced freely.
install_symlinks() {
  local plugins="$1"
  mkdir -p "$plugins"

  local src_dir target_dir
  src_dir="$(resolve_path "$DIR")"
  target_dir="$plugins/disable-sleep"

  local target_dir_resolved=""
  if [[ -e "$target_dir" || -L "$target_dir" ]]; then
    target_dir_resolved="$(resolve_path "$target_dir" 2>/dev/null || echo "")"
  fi

  if [[ "$src_dir" == "$target_dir_resolved" ]]; then
    : # already in place (Flow 1: install.sh downloaded files into $plugins/disable-sleep)
  elif [[ -L "$target_dir" || ! -e "$target_dir" ]]; then
    ln -sfn "$src_dir" "$target_dir"
    info "Linked: $target_dir -> $src_dir"
  else
    err "$target_dir exists and is a real directory with unrelated content; refusing to overwrite."
    err "Move or remove it manually, then re-run."
    exit 1
  fi

  # Top-level entry — relative so it works for both Flow 1 and Flow 2.
  ln -sf "disable-sleep/disable-sleep.10s.sh" "$plugins/disable-sleep.10s.sh"
}

# Inverse of install_symlinks. Only removes things that are symlinks, never
# a real directory — that protects Flow 1's downloaded files until install.sh
# --uninstall (which rm -rf's the dir) takes care of them.
uninstall_symlinks() {
  local plugins="$1"
  if [[ -L "$plugins/disable-sleep.10s.sh" ]]; then
    rm -f "$plugins/disable-sleep.10s.sh"
    info "Unlinked: $plugins/disable-sleep.10s.sh"
  fi
  if [[ -L "$plugins/disable-sleep" ]]; then
    rm -f "$plugins/disable-sleep"
    info "Unlinked: $plugins/disable-sleep"
  fi
}

# Re-exec self as root. From a terminal we use plain sudo (with -p so the
# user sees *why* we need a password); from xbar's menu (no TTY) we use the
# macOS GUI dialog via osascript and pass the reason as `with prompt`.
#
# Args: <reason> <remaining setup.sh args...>
escalate() {
  local reason="$1"; shift
  if [[ -t 0 ]]; then
    info ""
    info "→ $reason"
    exec sudo -p "[xbar disable-sleep] $reason — password: " "$SELF" "$@"
  fi
  local sh_cmd
  sh_cmd="$(printf '%q ' "$SELF" "$@")"
  local as_str="${sh_cmd//\\/\\\\}"
  as_str="${as_str//\"/\\\"}"
  local prompt_str="${reason//\\/\\\\}"
  prompt_str="${prompt_str//\"/\\\"}"
  osascript -e "do shell script \"$as_str\" with prompt \"$prompt_str\" with administrator privileges" >/dev/null
  exit $?
}

# Write /etc/sudoers.d/xbar-disable-sleep with an exact two-command allow-list,
# validated by visudo BEFORE the destination is touched.
install_sudoers() {
  local target_user="${1:-}"
  if [[ -z "$target_user" || "$target_user" == "root" ]]; then
    err "install_sudoers: empty/root username."
    exit 2
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
    err "visudo validation failed; sudoers not installed."
    exit 1
  fi

  /usr/bin/install -m 0440 -o root -g wheel "$tmp" "$SUDOERS_FILE"
  rm -f "$tmp"
  info "Installed: $SUDOERS_FILE (user=$target_user)"
}

uninstall_sudoers() {
  if [[ -f "$SUDOERS_FILE" ]]; then
    rm -f "$SUDOERS_FILE"
    info "Removed: $SUDOERS_FILE"
  else
    info "Already absent: $SUDOERS_FILE"
  fi
}

cmd_install() {
  local target_user="${1:-${SUDO_USER:-$USER}}"

  if [[ -z "$target_user" || "$target_user" == "root" ]]; then
    err "Refusing to install rule for empty/root user; pass a username explicitly."
    exit 2
  fi

  # User-space step: symlinks. Done BEFORE any escalation so admin privileges
  # are limited strictly to writing /etc/sudoers.d.
  local plugins
  plugins="$(plugins_dir_for "$target_user")"
  install_symlinks "$plugins"

  # Sudoers step needs root.
  if [[ "$EUID" -ne 0 ]]; then
    escalate "Install sudoers rule so xbar can toggle sleep without a password." \
      _install_sudoers "$target_user"
    exit 1  # unreachable; escalate exec's or exits
  fi

  install_sudoers "$target_user"
}

cmd_uninstall() {
  local target_user="${SUDO_USER:-$USER}"

  if [[ -n "$target_user" && "$target_user" != "root" ]]; then
    local plugins
    plugins="$(plugins_dir_for "$target_user")"
    uninstall_symlinks "$plugins"
  fi

  if [[ "$EUID" -ne 0 ]]; then
    escalate "Remove the sudoers rule for xbar's sleep toggle." _uninstall_sudoers
    exit 1
  fi

  uninstall_sudoers
}

cmd_status() {
  local target_user="${SUDO_USER:-$USER}"
  local plugins
  plugins="$(plugins_dir_for "$target_user")"

  local all_ok=0

  printf 'plugins dir: %s\n' "$plugins"

  if [[ -L "$plugins/disable-sleep" ]]; then
    printf 'plugin dir : symlink -> %s\n' "$(readlink "$plugins/disable-sleep")"
  elif [[ -d "$plugins/disable-sleep" ]]; then
    printf 'plugin dir : real directory (installed by install.sh)\n'
  else
    printf 'plugin dir : MISSING\n'
    all_ok=1
  fi

  if [[ -L "$plugins/disable-sleep.10s.sh" ]]; then
    printf 'top entry  : symlink -> %s\n' "$(readlink "$plugins/disable-sleep.10s.sh")"
  else
    printf 'top entry  : MISSING\n'
    all_ok=1
  fi

  if [[ -f "$SUDOERS_FILE" ]]; then
    printf 'sudoers    : installed at %s\n' "$SUDOERS_FILE"
  else
    printf 'sudoers    : MISSING\n'
    all_ok=1
  fi

  exit "$all_ok"
}

usage() {
  cat <<USAGE
Usage:
  $0 [install [username]]  # symlinks + sudoers (default; current user)
  $0 uninstall             # remove both symlinks and sudoers
  $0 status                # show what's installed where

Env:
  XBAR_PLUGINS_DIR=<path>  override xbar plugins folder
USAGE
}

case "${1:-install}" in
  install)             shift; cmd_install   "$@" ;;
  uninstall)           shift; cmd_uninstall "$@" ;;
  status)              shift; cmd_status    "$@" ;;
  # internal — called by escalate() so the elevated re-entry only touches
  # /etc/sudoers.d, never user-owned paths under /var/root.
  _install_sudoers)
    shift
    if [[ "$EUID" -ne 0 ]]; then
      escalate "Install sudoers rule so xbar can toggle sleep without a password." \
        _install_sudoers "$@"
      exit 1
    fi
    install_sudoers "$@"
    ;;
  _uninstall_sudoers)
    shift
    if [[ "$EUID" -ne 0 ]]; then
      escalate "Remove the sudoers rule for xbar's sleep toggle." _uninstall_sudoers
      exit 1
    fi
    uninstall_sudoers
    ;;
  help|-h|--help)      usage ;;
  *) err "Unknown subcommand: $1"; usage >&2; exit 2 ;;
esac
