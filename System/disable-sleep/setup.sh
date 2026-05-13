#!/usr/bin/env bash
# setup.sh — symlink the disable-sleep plugin into xbar's plugins folder.
#
# Run: ./setup.sh
# Env: XBAR_PLUGINS_DIR overrides target (else ~/Library/Application Support/xbar/plugins)
#
# No sudo, no prompts. For the passwordless toggle (sudoers rule), run
# ./passwordless.sh — or click the menu item once the plugin is loaded.

set -u

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"
SCRIPT="$DIR/disable-sleep.10s.sh"
LINK="$PLUGINS/disable-sleep.10s.sh"

mkdir -p "$PLUGINS"

# Migration: previous versions left a `disable-sleep` entry (symlink to dir,
# or real directory from old install.sh) next to the script symlink. xbar
# tried to fork/exec it and crashed. Sweep it out if safe.
LEGACY="$PLUGINS/disable-sleep"
if [[ -L "$LEGACY" ]]; then
  rm -f "$LEGACY"
  echo "Removed legacy entry: $LEGACY"
elif [[ -d "$LEGACY" ]]; then
  expected='^(disable-sleep\.10s\.sh|setup\.sh|uninstall\.sh|passwordless\.sh|install\.sh|bed\.png|bed-no\.png|make-icons\.py)$'
  extras="$(/bin/ls -A "$LEGACY" | grep -Ev "$expected" || true)"
  if [[ -z "$extras" ]]; then
    rm -rf "$LEGACY"
    echo "Removed legacy directory: $LEGACY"
  else
    echo "WARNING: $LEGACY contains unrelated files — leaving it alone:" >&2
    echo "$extras" >&2
    echo "(xbar may keep erroring on it until you move/remove it manually.)" >&2
  fi
fi

ln -sfn "$SCRIPT" "$LINK"
echo "Linked: $LINK -> $SCRIPT"
