#!/usr/bin/env bash
# install.sh — one-shot bootstrap for the disable-sleep xbar plugin.
#
# Pipe directly from GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/install.sh | bash
#
# What it does:
#   1. Downloads plugin + setup.sh + icons into the ASSETS dir (default
#      ~/Library/Application Support/xbar-disable-sleep) — NOT into xbar's
#      plugins folder, because xbar treats every entry there as a plugin to
#      fork/exec and would error on a subdirectory.
#   2. Delegates to setup.sh install, which places one symlink in xbar's
#      plugins folder and installs the sudoers rule.
#   3. Pokes xbar to refresh.
#
# For dev / clone-based installs, skip install.sh entirely and run
# `./setup.sh` from inside your checkout — same result, no download.
#
# Flags:
#   --dir <path>     override xbar plugins folder (else $XBAR_PLUGINS_DIR or
#                    ~/Library/Application Support/xbar/plugins)
#   --assets <path>  override where support files live (else
#                    $XBAR_DISABLE_SLEEP_DIR or
#                    ~/Library/Application Support/xbar-disable-sleep)
#   --skip-setup     download files only, don't run setup.sh install (xbar
#                    won't see the plugin until you do so manually)
#   --uninstall      reverse: setup.sh uninstall + rm -rf the assets dir
#   --help, -h       show this help
#
# Environment overrides (for testing the bootstrapper against a fork/branch):
#   REPO=user/repo      default kiprasmel/xbar-plugins
#   REF=branch-or-sha   default main

set -euo pipefail

REPO="${REPO:-kiprasmel/xbar-plugins}"
REF="${REF:-main}"
SUBPATH="System/disable-sleep"
BASE_URL="https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH"

DEFAULT_PLUGINS="$HOME/Library/Application Support/xbar/plugins"
TARGET="${XBAR_PLUGINS_DIR:-$DEFAULT_PLUGINS}"

DEFAULT_ASSETS="$HOME/Library/Application Support/xbar-disable-sleep"
ASSETS="${XBAR_DISABLE_SLEEP_DIR:-$DEFAULT_ASSETS}"

action="install"
skip_setup=0

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dir)         TARGET="$2"; shift 2 ;;
    --assets)      ASSETS="$2"; shift 2 ;;
    --skip-setup)  skip_setup=1; shift ;;
    --uninstall)   action="uninstall"; shift ;;
    --help|-h)     usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ "$(uname)" != "Darwin" ]]; then
  echo "macOS only." >&2
  exit 1
fi

# Migration: previous versions of install.sh put everything in
# $TARGET/disable-sleep/. That subdirectory is what xbar was choking on.
# Sweep it (and its symlink twin from old setup.sh) out before we go further.
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
  if [[ -x "$ASSETS/setup.sh" ]]; then
    XBAR_PLUGINS_DIR="$TARGET" "$ASSETS/setup.sh" uninstall || true
  fi
  rm -f  "$TARGET/disable-sleep.10s.sh"
  rm -rf "$ASSETS"
  migrate_legacy
  echo "Uninstalled disable-sleep."
  exit 0
fi

migrate_legacy
mkdir -p "$ASSETS"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT

echo "Downloading from $BASE_URL into $ASSETS/ ..."
for f in disable-sleep.10s.sh setup.sh bed.png bed-no.png; do
  curl -fsSL --proto '=https' --tlsv1.2 --retry 3 --retry-delay 1 \
    -o "$stage/$f" "$BASE_URL/$f"
done

chmod +x "$stage/disable-sleep.10s.sh" "$stage/setup.sh"

mv "$stage"/disable-sleep.10s.sh "$ASSETS/disable-sleep.10s.sh"
mv "$stage"/setup.sh             "$ASSETS/setup.sh"
mv "$stage"/bed.png              "$ASSETS/bed.png"
mv "$stage"/bed-no.png           "$ASSETS/bed-no.png"

if [[ "$skip_setup" -eq 0 ]]; then
  echo "Running setup.sh install (links plugin into xbar + installs sudoers rule; you'll be prompted for your password) ..."
  XBAR_PLUGINS_DIR="$TARGET" "$ASSETS/setup.sh" install || {
    echo "setup.sh install failed. Re-run manually:"
    echo "  XBAR_PLUGINS_DIR=$(printf %q "$TARGET") $ASSETS/setup.sh install"
  }
fi

# Nudge xbar to pick up the new plugin. Best-effort; harmless if xbar isn't running.
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
