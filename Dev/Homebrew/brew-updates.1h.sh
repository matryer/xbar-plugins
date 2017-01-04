#!/bin/bash
# <bitbar.title>Homebrew Updates</bitbar.title>
# <bitbar.author>killercup</bitbar.author>
# <bitbar.author.github>killercup</bitbar.author.github>
# <bitbar.desc>List available updates from Homebrew (OS X)</bitbar.desc>

exit_with_error() {
  echo "err | color=red";
  exit 1;
}

/usr/local/bin/brew update > /dev/null || exit_with_error;

PINNED=$(/usr/local/bin/brew list --pinned);
OUTDATED=$(/usr/local/bin/brew outdated --quiet);

UPDATES=$(comm -13 <(for X in "${PINNED[@]}"; do echo "${X}"; done) <(for X in "${OUTDATED[@]}"; do echo "${X}"; done))

UPDATE_COUNT=$(echo "$UPDATES" | grep -c '[^[:space:]]');

echo "↑$UPDATE_COUNT | dropdown=false"
echo "---";
if [ -n "$UPDATES" ]; then
  echo "Upgrade all | bash=/usr/local/bin/brew param1=upgrade terminal=false refresh=true"
  echo "$UPDATES" | awk '{print $0 " | terminal=false refresh=true bash=/usr/local/bin/brew param1=upgrade param2=" $1}'
fi
