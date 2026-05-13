#!/usr/bin/env bash
# install.sh — one-shot bootstrap for the disable-sleep xbar plugin.
#
# Pipe directly from GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/install.sh | bash
#
# What it does:
#   1. Downloads support files into the ASSETS dir (default
#      ~/Library/Application Support/xbar-disable-sleep) — NOT into xbar's
#      plugins folder, because xbar would try to fork/exec any subdirectory
#      there and break.
#   2. Runs setup.sh, which symlinks the plugin script into xbar's plugins
#      folder.
#   3. Unless --no-passwordless, runs passwordless.sh to install the sudoers
#      rule (you'll be prompted for your password — once).
#   4. Pokes xbar to refresh.
#
# For dev / clone-based installs, skip install.sh entirely and run
# `./setup.sh` (and optionally `./passwordless.sh`) from your checkout.
#
# Flags:
#   --dir <path>         override xbar plugins folder (else $XBAR_PLUGINS_DIR
#                        or ~/Library/Application Support/xbar/plugins)
#   --assets <path>      override the support-files dir (else
#                        $XBAR_DISABLE_SLEEP_DIR or
#                        ~/Library/Application Support/xbar-disable-sleep)
#   --no-passwordless    skip the sudoers step (no password prompt — handy
#                        for tests; the plugin still works, just prompts on
#                        every toggle until you run ./passwordless.sh)
#   --uninstall          reverse: uninstall.sh + passwordless.sh uninstall +
#                        rm -rf the assets dir
#   --help, -h           show this help
#
# Environment overrides (for testing the bootstrapper against a fork/branch):
#   REPO=user/repo       default kiprasmel/xbar-plugins
#   REF=branch-or-sha    default main
#   BASE_URL=<url>       override the file-fetch base entirely (overrides
#                        REPO/REF; useful for local http servers in tests)

set -euo pipefail

REPO="${REPO:-kiprasmel/xbar-plugins}"
REF="${REF:-main}"
SUBPATH="System/disable-sleep"
BASE_URL="${BASE_URL:-https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH}"

DEFAULT_PLUGINS="$HOME/Library/Application Support/xbar/plugins"
TARGET="${XBAR_PLUGINS_DIR:-$DEFAULT_PLUGINS}"

DEFAULT_ASSETS="$HOME/Library/Application Support/xbar-disable-sleep"
ASSETS="${XBAR_DISABLE_SLEEP_DIR:-$DEFAULT_ASSETS}"

action="install"
no_passwordless=0

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dir)              TARGET="$2"; shift 2 ;;
    --assets)           ASSETS="$2"; shift 2 ;;
    --no-passwordless)  no_passwordless=1; shift ;;
    --uninstall)        action="uninstall"; shift ;;
    --help|-h)          usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ "$(uname)" != "Darwin" ]]; then
  echo "macOS only." >&2
  exit 1
fi

# Migration: older versions stashed files in $TARGET/disable-sleep/ — xbar
# choked on that subdirectory. Sweep it (and its symlink twin) out.
migrate_legacy() {
  if [[ -L "$TARGET/disable-sleep" ]]; then
    rm -f "$TARGET/disable-sleep"
    echo "Removed legacy symlink: $TARGET/disable-sleep"
  elif [[ -d "$TARGET/disable-sleep" ]]; then
    rm -rf "$TARGET/disable-sleep"
    echo "Removed legacy directory: $TARGET/disable-sleep"
  fi
}

if [[ "$action" == "uninstall" ]]; then
  if [[ -x "$ASSETS/uninstall.sh" ]]; then
    XBAR_PLUGINS_DIR="$TARGET" "$ASSETS/uninstall.sh" || true
  fi
  if [[ "$no_passwordless" -eq 0 && -x "$ASSETS/passwordless.sh" ]]; then
    "$ASSETS/passwordless.sh" uninstall || true
  fi
  rm -rf "$ASSETS"
  migrate_legacy
  echo "Uninstalled disable-sleep."
  exit 0
fi

migrate_legacy
mkdir -p "$ASSETS"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT

files=(disable-sleep.10s.sh setup.sh uninstall.sh passwordless.sh bed.png bed-no.png)
echo "Downloading from $BASE_URL into $ASSETS/ ..."
for f in "${files[@]}"; do
  curl -fsSL --proto '=https' --tlsv1.2 --retry 3 --retry-delay 1 \
    -o "$stage/$f" "$BASE_URL/$f"
done

chmod +x "$stage/disable-sleep.10s.sh" \
         "$stage/setup.sh" \
         "$stage/uninstall.sh" \
         "$stage/passwordless.sh"

for f in "${files[@]}"; do
  mv "$stage/$f" "$ASSETS/$f"
done

XBAR_PLUGINS_DIR="$TARGET" "$ASSETS/setup.sh"

if [[ "$no_passwordless" -eq 0 ]]; then
  echo "Setting up passwordless toggle (sudoers rule; you'll be prompted for your password)..."
  "$ASSETS/passwordless.sh" install || {
    echo "passwordless.sh install failed. The plugin still works, but every"
    echo "toggle will prompt for your password. Re-run later with:"
    echo "  $ASSETS/passwordless.sh install"
  }
fi

# Best-effort xbar refresh; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

cat <<DONE
Done.
  assets:  $ASSETS/
  plugin:  $TARGET/disable-sleep.10s.sh -> $ASSETS/disable-sleep.10s.sh

A bed icon should appear in your menubar shortly (if xbar is running). Click it,
then click the toggle button to flip pmset -b disablesleep.

To uninstall:
  bash <(curl -fsSL $BASE_URL/install.sh) --uninstall
DONE
