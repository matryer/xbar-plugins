#!/usr/bin/env bash
# install.sh — bootstrap the disable-sleep xbar plugin.
#
# From GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/install.sh | bash
#
# Or, from a local clone: just run ./setup.sh instead.
#
# Env overrides (for testing):
#   REPO=user/repo                 default kiprasmel/xbar-plugins
#   REF=branch-or-sha              default main
#   BASE_URL=<url>                 override the fetch base entirely
#   XBAR_PLUGINS_DIR=<path>        default ~/Library/Application Support/xbar/plugins
#   XBAR_DISABLE_SLEEP_DIR=<path>  default ~/Library/Application Support/xbar-disable-sleep

set -euo pipefail

REPO="${REPO:-kiprasmel/xbar-plugins}"
REF="${REF:-main}"
SUBPATH="System/disable-sleep"
BASE_URL="${BASE_URL:-https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH}"

PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"
ASSETS="${XBAR_DISABLE_SLEEP_DIR:-$HOME/Library/Application Support/xbar-disable-sleep}"

if [[ "$(uname)" != "Darwin" ]]; then
  echo "macOS only." >&2
  exit 1
fi

# Migration: older versions stashed files in $PLUGINS/disable-sleep/. xbar
# choked on that subdirectory; sweep it out.
if [[ -e "$PLUGINS/disable-sleep" || -L "$PLUGINS/disable-sleep" ]]; then
  rm -rf "$PLUGINS/disable-sleep"
  echo "Removed legacy entry: $PLUGINS/disable-sleep"
fi

# Wipe the assets dir so stale files (e.g. an old passwordless.sh) don't
# linger after an upgrade.
rm -rf "$ASSETS"
mkdir -p "$ASSETS"

stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT

files=(disable-sleep.10s.sh setup.sh uninstall.sh bed.png bed-no.png)
echo "Downloading from $BASE_URL into $ASSETS/ ..."
for f in "${files[@]}"; do
  curl -fsSL --proto '=https' --tlsv1.2 --retry 3 --retry-delay 1 \
    -o "$stage/$f" "$BASE_URL/$f"
done

chmod +x "$stage/disable-sleep.10s.sh" "$stage/setup.sh" "$stage/uninstall.sh"

for f in "${files[@]}"; do
  mv "$stage/$f" "$ASSETS/$f"
done

XBAR_PLUGINS_DIR="$PLUGINS" "$ASSETS/setup.sh"

# Best-effort xbar refresh; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

cat <<DONE
Done.
  assets: $ASSETS/
  plugin: $PLUGINS/disable-sleep.10s.sh

A bed icon should appear in xbar's menubar shortly. Click it to toggle
\`pmset -b disablesleep\`. You'll be prompted for your password on each
toggle unless you configure sudoers manually.

To uninstall:
  curl -fsSL $BASE_URL/uninstall.sh | bash
DONE
