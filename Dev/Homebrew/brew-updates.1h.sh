#!/bin/bash

####
# List available updates from Homebrew (OS X)
###

exit_with_error() {
  echo "err | color=red";
  exit 1;
}

/usr/local/bin/brew update > /dev/null || exit_with_error;

UPDATES=$(/usr/local/bin/brew outdated --verbose);

UPDATE_COUNT=$(echo "$UPDATES" | grep -c '[^[:space:]]');

echo "↑$UPDATE_COUNT | dropdown=false"
echo "---";
if [ -n "$UPDATES" ]; then
  echo "Upgrade all | bash=/usr/local/bin/brew param1=upgrade"
  echo "$UPDATES" | awk '{print $0 " | bash=/usr/local/bin/brew param1=upgrade param2=" $1 }'
fi
