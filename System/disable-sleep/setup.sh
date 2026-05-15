#!/usr/bin/env bash
# setup.sh — symlink the disable-sleep plugin into xbar's plugins folder, and
# install the NOPASSWD sudoers rule so toggles don't prompt on every click.
#
# Run: ./setup.sh
# Env:
#   XBAR_PLUGINS_DIR     overrides symlink target (default ~/Library/Application Support/xbar/plugins)
#   SKIP_SUDOERS_SETUP=1 skip the sudoers-setup step (CI / tests). Toggles will
#                        then fall back to a per-click osascript admin prompt.
#
# Prompts once for sudo (via terminal sudo or osascript) the first time
# sudoers-setup.sh runs; idempotent thereafter.

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

# Install the NOPASSWD sudoers rule so per-click osascript prompts go away.
# Idempotent: sudoers-setup.sh short-circuits if /etc/sudoers.d/xbar-disable-sleep
# already exists, so re-running setup.sh won't re-prompt.
# Failure here must not abort: the symlink already succeeded; users can re-run
# sudoers-setup.sh later. The fallback path in disable-sleep.10s.sh
# (osascript with administrator privileges) still works without the rule.
if [[ "${SKIP_SUDOERS_SETUP:-}" == "1" ]]; then
  echo "SKIP_SUDOERS_SETUP=1 set; skipping sudoers rule. You'll be prompted on every toggle."
else
  "$DIR/sudoers-setup.sh" install \
    || echo "WARNING: sudoers-setup failed; you'll be prompted on every toggle. Re-run later: $DIR/sudoers-setup.sh" >&2
fi
