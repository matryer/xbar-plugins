#!/usr/bin/env bash
# install.sh — one-shot bootstrap for the disable-sleep xbar plugin.
#
# Pipe directly from GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/install.sh | bash
#
# What it does:
#   1. Downloads plugin + helper + icons into <plugins-dir>/disable-sleep/
#   2. Symlinks the plugin to the plugins-dir top level so xbar finds it
#   3. Runs setup.sh install to add the sudoers rule (passwordless toggle)
#   4. Pokes xbar to refresh
#
# Flags:
#   --dir <path>     override xbar plugins folder (else $XBAR_PLUGINS_DIR or
#                    ~/Library/Application Support/xbar/plugins)
#   --skip-setup     don't run setup.sh install (you'll get an admin prompt
#                    on every toggle until you run it manually)
#   --uninstall      remove the sudoers rule, the symlink, and the subdir
#   --help, -h       show this help
#
# Environment overrides (for testing the bootstrapper against a fork/branch):
#   REPO=user/repo   default kiprasmel/xbar-plugins
#   REF=branch-or-sha   default main

set -euo pipefail

REPO="${REPO:-kiprasmel/xbar-plugins}"
REF="${REF:-main}"
SUBPATH="System/disable-sleep"
BASE_URL="https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH"

DEFAULT_DIR="$HOME/Library/Application Support/xbar/plugins"
TARGET="${XBAR_PLUGINS_DIR:-$DEFAULT_DIR}"

action="install"
skip_setup=0

usage() {
  sed -n '2,/^$/p' "$0" | sed 's/^# \?//'
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dir)         TARGET="$2"; shift 2 ;;
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

if [[ "$action" == "uninstall" ]]; then
  if [[ -x "$TARGET/disable-sleep/setup.sh" ]]; then
    "$TARGET/disable-sleep/setup.sh" uninstall || true
  fi
  rm -f  "$TARGET/disable-sleep.10s.sh"
  rm -rf "$TARGET/disable-sleep"
  echo "Uninstalled disable-sleep from: $TARGET"
  exit 0
fi

mkdir -p "$TARGET/disable-sleep"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT

echo "Downloading from $BASE_URL into $TARGET/disable-sleep/ ..."
for f in disable-sleep.10s.sh setup.sh bed.png bed-no.png; do
  curl -fsSL --proto '=https' --tlsv1.2 --retry 3 --retry-delay 1 \
    -o "$stage/$f" "$BASE_URL/$f"
done

chmod +x "$stage/disable-sleep.10s.sh" "$stage/setup.sh"

mv "$stage"/disable-sleep.10s.sh "$TARGET/disable-sleep/disable-sleep.10s.sh"
mv "$stage"/setup.sh             "$TARGET/disable-sleep/setup.sh"
mv "$stage"/bed.png              "$TARGET/disable-sleep/bed.png"
mv "$stage"/bed-no.png           "$TARGET/disable-sleep/bed-no.png"

# xbar's plugin discovery only looks at the top of its plugins folder, so
# expose the plugin there as a relative symlink into the subdir.
ln -sf "disable-sleep/disable-sleep.10s.sh" "$TARGET/disable-sleep.10s.sh"

if [[ "$skip_setup" -eq 0 ]]; then
  echo "Running setup.sh install (you'll be prompted for your password) ..."
  "$TARGET/disable-sleep/setup.sh" install || {
    echo "setup.sh install failed; the plugin still works but will prompt for"
    echo "your password on every toggle. Re-run manually:"
    echo "  $TARGET/disable-sleep/setup.sh install"
  }
fi

# Nudge xbar to pick up the new plugin. Best-effort; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

cat <<DONE
Done. Installed into:
  $TARGET/disable-sleep/

A bed icon should appear in your menubar shortly (if xbar is running). Click it,
then click the toggle button to flip pmset -b disablesleep.

To uninstall:
  bash <(curl -fsSL $BASE_URL/install.sh) --uninstall
DONE
