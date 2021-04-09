#!/usr/bin/env bash

# <xbar.title>Bandwidth</xbar.title>
# <xbar.version>v0.0.0</xbar.version>
# <xbar.author>Ant Cosentino</xbar.author>
# <xbar.author.github>skibz</xbar.author.github>
# <xbar.desc>Displays TX and RX bitrate of your main ethernet interface in the status bar and hides other interfaces in the context menu.</xbar.desc>
# <xbar.dependencies>ifstat</xbar.dependencies>
# <xbar.image>https://cloud.githubusercontent.com/assets/2462211/12748504/584bbcea-c9b3-11e5-8109-ad8fdcefdc75.png</xbar.image>

export PATH="/usr/local/bin:${PATH}"
INTERFACES=$(ifconfig -lu)

echo "▼ $(ifstat -n -w -i en0 0.1 1 | tail -n 1 | awk '{print $1, " - ", $2;}') ▲"
echo "---"
for INTERFACE in ${INTERFACES}; do
  if [[ ${INTERFACE} != "en0" ]]; then
    echo "${INTERFACE}: ▼ $(ifstat -n -w -i "${INTERFACE}" 0.1 1 | tail -n 1 | awk '{print $1, " - ", $2;}') ▲"
  fi
done
