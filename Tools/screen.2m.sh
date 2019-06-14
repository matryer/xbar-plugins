#!/usr/bin/env bash
#
# <bitbar.title>Screen</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jake Gage</bitbar.author>
# <bitbar.author.github>Dispader</bitbar.author.github>
# <bitbar.desc>Monitor, attach, and power detach from GNU Screen sessions.</bitbar.desc>
# <bitbar.dependencies>screen</bitbar.dependencies>
# <bitbar.image>https://user-images.githubusercontent.com/2664155/54407949-f5490280-46ad-11e9-86fc-9856d64b5a0e.png</bitbar.image>
# <bitbar.abouturl>http://github.com/Dispader/screen-bitbar-plugin</bitbar.abouturl

set -eu
PATH="/usr/local/bin:$PATH" SCREEN_COMMAND=$(command -v screen)
echo "💻"
echo '---'
SCREENS=$(${SCREEN_COMMAND} -list | grep -o '\s*.*\s*(.*)')
if [[ -z ${SCREENS} ]]; then
  echo "no screens"
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
