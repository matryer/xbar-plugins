#!/usr/bin/env bash
# uninstall.sh — remove the disable-sleep xbar plugin.
#
# From GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/uninstall.sh | bash
#
# Or, locally from a clone or installed copy: just run ./uninstall.sh.
#
# Env overrides (for testing):
#   XBAR_PLUGINS_DIR=<path>        default ~/Library/Application Support/xbar/plugins
#   XBAR_DISABLE_SLEEP_DIR=<path>  default ~/Library/Application Support/xbar-disable-sleep

set -u

PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"
ASSETS="${XBAR_DISABLE_SLEEP_DIR:-$HOME/Library/Application Support/xbar-disable-sleep}"

for entry in disable-sleep.10s.sh disable-sleep; do
  if [[ -L "$PLUGINS/$entry" ]]; then
    rm -f "$PLUGINS/$entry"
    echo "Removed: $PLUGINS/$entry"
  elif [[ -d "$PLUGINS/$entry" ]]; then
    # Legacy real directory from old install.sh.
    rm -rf "$PLUGINS/$entry"
    echo "Removed: $PLUGINS/$entry"
  fi
done

if [[ -d "$ASSETS" ]]; then
  rm -rf "$ASSETS"
  echo "Removed: $ASSETS"
fi

# Best-effort xbar refresh; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true
