#!/usr/bin/env bash
# uninstall.sh — remove the disable-sleep xbar plugin.
#
# From GitHub:
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/uninstall.sh | bash
#
# Or, locally from a clone or installed copy: just run ./uninstall.sh.
#
# This removes the plugin's symlink (and any legacy install artifacts) from
# xbar's plugins folder. It does NOT delete your git clone — `rm -rf` it
# yourself if you want to.
#
# Env overrides (for testing):
#   XBAR_PLUGINS_DIR=<path>        default ~/Library/Application Support/xbar/plugins
#   XBAR_DISABLE_SLEEP_DIR=<path>  default ~/Library/Application Support/xbar-disable-sleep

set -u

PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"
LEGACY_ASSETS="${XBAR_DISABLE_SLEEP_DIR:-$HOME/Library/Application Support/xbar-disable-sleep}"

removed_anything=0

for entry in disable-sleep.10s.sh disable-sleep; do
  if [[ -L "$PLUGINS/$entry" ]]; then
    rm -f "$PLUGINS/$entry"
    echo "Removed: $PLUGINS/$entry"
    removed_anything=1
  elif [[ -d "$PLUGINS/$entry" ]]; then
    # Legacy real directory from the very first install.sh.
    rm -rf "${PLUGINS:?}/$entry"
    echo "Removed: $PLUGINS/$entry"
    removed_anything=1
  fi
done

# Legacy assets dir from the curl-files install.sh (pre clone-based flow).
if [[ -d "$LEGACY_ASSETS" ]]; then
  rm -rf "$LEGACY_ASSETS"
  echo "Removed legacy assets dir: $LEGACY_ASSETS"
  removed_anything=1
fi

# Best-effort xbar refresh; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

if [[ "$removed_anything" -eq 0 ]]; then
  echo "Nothing to remove. (No disable-sleep plugin found in $PLUGINS.)"
fi

cat <<NOTE

If you installed via install.sh, your git clone is wherever you ran it
(default ./xbar-plugins). It is left in place — \`rm -rf\` it manually
if you no longer want it.
NOTE
