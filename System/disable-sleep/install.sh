#!/usr/bin/env bash
# install.sh — clone xbar-plugins and wire up the disable-sleep plugin.
#
# From GitHub (clones into ./xbar-plugins in the current working dir):
#   curl -fsSL https://raw.githubusercontent.com/kiprasmel/xbar-plugins/main/System/disable-sleep/install.sh | bash
#
# Or, from a local clone: just run ./install.sh (or ./setup.sh) — when the
# script detects it is already inside a clone of this repo, it skips the
# clone/pull entirely and just runs setup.sh.
#
# Env overrides (for testing / forks):
#   REPO=user/repo                 default kiprasmel/xbar-plugins
#   REF=branch-or-sha              default main
#   CLONE_DIR=<path>               default $PWD/xbar-plugins
#   XBAR_PLUGINS_DIR=<path>        default ~/Library/Application Support/xbar/plugins
#   FORCE_CLONE=1                  bypass the local-execution shortcut
#   SKIP_SUDOERS_SETUP=1           skip the sudoers-setup step (CI / tests).
#                                  Toggles will then prompt for a password every
#                                  click via osascript.

set -euo pipefail

REPO="${REPO:-kiprasmel/xbar-plugins}"
REF="${REF:-main}"
SUBPATH="System/disable-sleep"

PLUGINS="${XBAR_PLUGINS_DIR:-$HOME/Library/Application Support/xbar/plugins}"
LEGACY_ASSETS="$HOME/Library/Application Support/xbar-disable-sleep"

if [[ "$(uname)" != "Darwin" ]]; then
  echo "macOS only." >&2
  exit 1
fi

if ! command -v git >/dev/null 2>&1; then
  cat >&2 <<EOF
git is required but not installed.
Install via:
  xcode-select --install     # ships Apple's git
  brew install git           # or via Homebrew
EOF
  exit 1
fi

# Migration: older versions stashed files in $PLUGINS/disable-sleep/. xbar
# choked on that subdirectory; sweep it out.
if [[ -e "$PLUGINS/disable-sleep" || -L "$PLUGINS/disable-sleep" ]]; then
  rm -rf "$PLUGINS/disable-sleep"
  echo "Removed legacy entry: $PLUGINS/disable-sleep"
fi

# Migration: previous installer copied loose files into ~/Library/Application
# Support/xbar-disable-sleep/. With the clone-based flow it's dead weight; reap it.
if [[ -d "$LEGACY_ASSETS" ]]; then
  rm -rf "$LEGACY_ASSETS"
  echo "Removed legacy assets dir: $LEGACY_ASSETS"
fi

# Local-execution shortcut: if invoked as a real file (not piped from
# `curl | bash`, where BASH_SOURCE[0] is empty) and the surrounding directory
# looks like a clone of this repo, skip the network entirely and just run
# setup.sh in place. Honors FORCE_CLONE=1 to bypass for testing.
if [[ "${FORCE_CLONE:-}" != "1" && -n "${BASH_SOURCE[0]:-}" ]]; then
  SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
  if [[ -x "$SELF_DIR/setup.sh" && -f "$SELF_DIR/disable-sleep.10s.sh" ]] \
     && TOPLEVEL="$(git -C "$SELF_DIR" rev-parse --show-toplevel 2>/dev/null)"; then
    echo "Detected existing clone at: $TOPLEVEL"
    echo "Skipping git clone (set FORCE_CLONE=1 to bypass)."

    XBAR_PLUGINS_DIR="$PLUGINS" "$SELF_DIR/setup.sh"

    open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

    cat <<DONE
Done.
  clone:  $TOPLEVEL
  plugin: $PLUGINS/disable-sleep.10s.sh

A bed icon should appear in xbar's menubar shortly. Click it to toggle
\`pmset -b disablesleep\`.

Sleep toggling needs root. setup.sh installs a NOPASSWD sudoers rule
(/etc/sudoers.d/xbar-disable-sleep) so toggles run without a prompt — you
should have been asked once for your sudo password above. If you skipped it
(SKIP_SUDOERS_SETUP=1) or it failed, your first menubar click will prompt
for your password to install the rule — then no more prompts. To (re)install
the rule manually:
  "$SELF_DIR/sudoers-setup.sh"
To remove it:
  sudo rm /etc/sudoers.d/xbar-disable-sleep

Edit the plugin in place:
  \$EDITOR "$SELF_DIR/disable-sleep.10s.sh"   # xbar reloads on its 10s tick

Update:
  cd "$TOPLEVEL" && git pull

To uninstall:
  curl -fsSL https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH/uninstall.sh | bash
DONE
    exit 0
  fi
fi

CLONE_DIR="${CLONE_DIR:-$PWD/xbar-plugins}"
REMOTE_URL="https://github.com/$REPO.git"

if [[ -d "$CLONE_DIR/.git" ]]; then
  existing_url="$(git -C "$CLONE_DIR" remote get-url origin 2>/dev/null || echo '')"
  # Loose match: tolerate https/ssh and trailing .git differences.
  norm_existing="$(echo "$existing_url" | sed -E 's#(\.git)?/?$##; s#git@github.com:#https://github.com/#')"
  norm_target="$(echo "$REMOTE_URL"     | sed -E 's#(\.git)?/?$##')"
  if [[ "$norm_existing" != "$norm_target" ]]; then
    cat >&2 <<EOF
$CLONE_DIR is a git repo, but its 'origin' is:
  $existing_url
Expected:
  $REMOTE_URL

Refusing to clobber it. Remove/move it, or set CLONE_DIR=<other-path> and re-run.
EOF
    exit 1
  fi

  echo "Updating existing clone: $CLONE_DIR"
  git -C "$CLONE_DIR" fetch origin "$REF"

  if [[ -n "$(git -C "$CLONE_DIR" status --porcelain)" ]]; then
    echo "WARNING: working tree at $CLONE_DIR has local changes — skipping checkout/pull." >&2
    echo "         Re-running setup.sh against the current state." >&2
  else
    git -C "$CLONE_DIR" checkout "$REF"
    # ff-only is harmless when REF is a SHA/tag (no upstream) — swallow the error.
    git -C "$CLONE_DIR" pull --ff-only 2>/dev/null || true
  fi
elif [[ -e "$CLONE_DIR" ]]; then
  echo "$CLONE_DIR exists and is not a git repo. Refusing to clobber." >&2
  echo "Remove/move it, or set CLONE_DIR=<other-path> and re-run." >&2
  exit 1
else
  echo "Cloning $REMOTE_URL (ref: $REF) into $CLONE_DIR ..."
  # Try shallow-clone-with-branch first (works for branches/tags); on failure
  # (e.g. REF is a SHA), fall back to full clone + checkout.
  if ! git clone --depth 1 --branch "$REF" "$REMOTE_URL" "$CLONE_DIR" 2>/dev/null; then
    git clone "$REMOTE_URL" "$CLONE_DIR"
    git -C "$CLONE_DIR" checkout "$REF"
  fi
fi

XBAR_PLUGINS_DIR="$PLUGINS" "$CLONE_DIR/$SUBPATH/setup.sh"

# Best-effort xbar refresh; harmless if xbar isn't running.
open 'xbar://app.xbarapp.com/refreshAllPlugins' >/dev/null 2>&1 || true

cat <<DONE
Done.
  clone:  $CLONE_DIR
  plugin: $PLUGINS/disable-sleep.10s.sh

A bed icon should appear in xbar's menubar shortly. Click it to toggle
\`pmset -b disablesleep\`.

Sleep toggling needs root. setup.sh installs a NOPASSWD sudoers rule
(/etc/sudoers.d/xbar-disable-sleep) so toggles run without a prompt — you
should have been asked once for your sudo password above. If you skipped it
(SKIP_SUDOERS_SETUP=1) or it failed, your first menubar click will prompt
for your password to install the rule — then no more prompts. To (re)install
the rule manually:
  "$CLONE_DIR/$SUBPATH/sudoers-setup.sh"
To remove it:
  sudo rm /etc/sudoers.d/xbar-disable-sleep

Edit the plugin in place:
  \$EDITOR "$CLONE_DIR/$SUBPATH/disable-sleep.10s.sh"   # xbar reloads on its 10s tick

Update:
  cd "$CLONE_DIR" && git pull

To uninstall:
  curl -fsSL https://raw.githubusercontent.com/$REPO/$REF/$SUBPATH/uninstall.sh | bash
DONE
