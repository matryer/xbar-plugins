#!/bin/bash
#
# <xbar.title>wakeonlan</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Shinya Fujino</xbar.author>
# <xbar.author.github>morinokami</xbar.author.github>
# <xbar.desc>Sends 'magic packets' to turn on a computer.</xbar.desc>
# <xbar.dependencies>wakeonlan</xbar.dependencies>

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
