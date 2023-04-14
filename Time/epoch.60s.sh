#!/usr/bin/env bash

# <xbar.title>Pretty Epoch Time</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Anthony Jarvis-Clark</xbar.author>
# <xbar.author.github>anthonyclarka2</xbar.author.github>
# <xbar.desc>Displays epoch (unix) time separated by commas.</xbar.desc>
# <xbar.image>http://i.imgur.com/ltLqOcy.png</xbar.image>
# <xbar.dependencies>bash,gnu-sed</xbar.dependencies>

BREWPATH=/usr/local/bin
if [[ $(sysctl -n machdep.cpu.brand_string) =~ "Apple" ]]; then
  BREWPATH=/opt/homebrew/bin
fi

EPOCHNOW=$(/bin/date +%s | ${BREWPATH}/gsed ':a;s/\B[0-9]\{3\}\>/,&/;ta')

echo "{" "${EPOCHNOW}" "}"
