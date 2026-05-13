#!/usr/bin/env bash
# uninstall.sh — unlink the disable-sleep plugin from xbar's plugins folder.
#
# Run: ./uninstall.sh
# Env: XBAR_PLUGINS_DIR overrides target (else ~/Library/Application Support/xbar/plugins)
#
# To also remove the passwordless-toggle sudoers rule, run:
#   ./passwordless.sh uninstall

set -u

PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"

for entry in disable-sleep.10s.sh disable-sleep; do
  if [[ -L "$PLUGINS/$entry" ]]; then
    rm -f "$PLUGINS/$entry"
    echo "Removed: $PLUGINS/$entry"
  fi
done
