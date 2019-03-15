#!/bin/bash
#
# <bitbar.title>Air Quality Index</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chongyu Yuan</bitbar.author>
# <bitbar.author.github>nnnggel</bitbar.author.github>
# <bitbar.desc>Real-time Air Quality Index</bitbar.desc>
# <bitbar.image>https://i.imgur.com/7bc5qqh.jpg</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>http://www.yuanchongyu.com</bitbar.abouturl>

MENUFONT="size=12 font=UbuntuMono-Bold"
COLORS=("#0ed812" "#ffde33" "#ff9933" "#cc0033" "#660099" "#7e0023")

# where to get the token -> https://aqicn.org/api/
TOKEN="xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
CITY="shanghai"

URL="http://aqicn.org/city/${CITY}/"

DATA=`curl -s http://api.waqi.info/feed/${CITY}/?token=${TOKEN}`
# how to install jq -> https://stedolan.github.io/jq/download/
AQI=`echo "${DATA}" | /usr/local/bin/jq '.data.aqi'`

function colorize {
  if [ "$1" -le 50 ]; then
    echo "${COLORS[0]}"
  elif [ "$1" -le 100 ]; then
    echo "${COLORS[1]}"
  elif [ "$1" -le 150 ]; then
    echo "${COLORS[2]}"
  elif [ "$1" -le 200 ]; then
    echo "${COLORS[3]}"
  elif [ "$1" -le 300 ]; then
    echo "${COLORS[4]}"
  else
    echo "${COLORS[5]}"
  fi
}

echo "😷${AQI} | color=$(colorize ${AQI}) ${MENUFONT}"

echo "---"
echo "Detail... | href=${URL}"
echo "Refresh... | refresh=true"