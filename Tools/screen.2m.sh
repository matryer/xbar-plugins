#!/usr/bin/env bash
#
# <xbar.title>Screen</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jake Gage</xbar.author>
# <xbar.author.github>Dispader</xbar.author.github>
# <xbar.desc>Monitor, attach, and power detach from GNU Screen sessions.</xbar.desc>
# <xbar.dependencies>screen</xbar.dependencies>
# <xbar.image>https://user-images.githubusercontent.com/2664155/54407949-f5490280-46ad-11e9-86fc-9856d64b5a0e.png</xbar.image>
# <xbar.abouturl>http://github.com/Dispader/screen-bitbar-plugin</xbar.abouturl

set -eu
PATH="/usr/local/bin:$PATH" SCREEN_COMMAND=$(command -v screen)
echo "💻"
echo '---'
SCREENS=$(${SCREEN_COMMAND} -list | grep -o '\s*.*\s*(.*)')
if [[ -z ${SCREENS} ]] || [[ ${SCREENS} =~ ^.*empty.*$ ]]; then
  echo "new screen session | refresh=true bash=${SCREEN_COMMAND}";
else
  (
    IFS=$'\n'; for LINE in $(screen -list); do
      if [[ ${LINE} =~ ^[[:space:]]+[[:digit:]]+\.(.+)[[:space:]]+(\(.*\))$ ]]
      then
        SCREEN_SESSION=${BASH_REMATCH[1]}
        SCREEN_SESSION_STATUS=${BASH_REMATCH[2]}
        if [[ "${SCREEN_SESSION_STATUS}" == "(Detached)" ]]
        then
          echo "⚫ ${SCREEN_SESSION} ${SCREEN_SESSION_STATUS} | refresh=true bash=${SCREEN_COMMAND} param1=-R param2=${SCREEN_SESSION}"
        else
          echo "🔵 ${SCREEN_SESSION} ${SCREEN_SESSION_STATUS}"
          echo "-- power detach | terminal=false refresh=true bash=${SCREEN_COMMAND} param1=-D param2=${SCREEN_SESSION} "
        fi
      fi
    done
  )
fi
