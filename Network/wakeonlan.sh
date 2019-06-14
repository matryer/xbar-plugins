#!/bin/bash
#
# <bitbar.title>wakeonlan</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Shinya Fujino</bitbar.author>
# <bitbar.author.github>morinokami</bitbar.author.github>
# <bitbar.desc>Sends 'magic packets' to turn on a computer.</bitbar.desc>
# <bitbar.dependencies>wakeonlan</bitbar.dependencies>

# Dependancies:
#   wakeonlan (brew install wakeonlan)

export PATH="/usr/local/bin:$PATH"

MAC_ADDRESS='01:23:45:67:89:AB' # Replace this with the target computer's MAC address
COMMAND_PATH=$(command -v wakeonlan)

echo 'WoL'
echo '---'

if [[ $COMMAND_PATH ]]; then
  echo "Turn on ${MAC_ADDRESS} | bash=/bin/bash param1=${COMMAND_PATH} param2=${MAC_ADDRESS} terminal=false"
else
  echo 'wakeonlan not installed'
fi
