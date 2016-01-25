#!/usr/bin/env bash

# <bitbar.title>Bandwidth</bitbar.title>
# <bitbar.version>v0.0.0</bitbar.version>
# <bitbar.author>Ant Cosentino</bitbar.author>
# <bitbar.author.github>skibz</bitbar.author.github>
# <bitbar.desc>Displays TX and RX bitrate of your main ethernet interface in the status bar and hides other interfaces in the context menu.</bitbar.desc>
# <bitbar.dependencies>node</bitbar.dependencies>

export PATH="/usr/local/bin:${PATH}"
INTERFACES=$(ifconfig -lu)

echo "▼ $(ifstat -n -w -i en0 0.1 1 | tail -n 1 | awk '{print $1, " - ", $2;}') ▲"
echo "---"
for INTERFACE in ${INTERFACES}; do
  if [[ ${INTERFACE} != "en0" ]]; then
    echo "${INTERFACE}: ▼ $(ifstat -n -w -i ${INTERFACE} 0.1 1 | tail -n 1 | awk '{print $1, " - ", $2;}') ▲"
  fi
done
