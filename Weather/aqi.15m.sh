#!/bin/bash
#
# <bitbar.title>Air Quality Index</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chongyu Yuan</bitbar.author>
# <bitbar.author.github>nnnggel</bitbar.author.github>
# <bitbar.desc>Real-time Air Quality Index, you need an 'aqi api token' and install 'jq' first.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/7bc5qqh.jpg</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>http://www.yuanchongyu.com</bitbar.abouturl>

MENUFONT="size=12 font=UbuntuMono-Bold"
COLORS=("#0ed812" "#ffde33" "#ff9933" "#cc0033" "#660099" "#7e0023" "#404040")

# where to get the token -> https://aqicn.org/api/
TOKEN="xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
CITY="shanghai"

URL="http://aqicn.org/city/${CITY}/"

DATA=$(curl -s http://api.waqi.info/feed/${CITY}/?token=${TOKEN})
# DELETE ME, TEST DATA
DATA="{\"status\":\"ok\",\"data\":{\"aqi\":824,\"idx\":1437,\"attributions\":[{\"url\":\"http://www.semc.gov.cn/\",\"name\":\"Shanghai Environment Monitoring Center(上海市环境监测中心)\"},{\"url\":\"http://106.37.208.233:20035/emcpublish/\",\"name\":\"China National Urban air quality real-time publishing platform (全国城市空气质量实时发布平台)\"},{\"url\":\"https://china.usembassy-china.org.cn/embassy-consulates/shanghai/air-quality-monitor-stateair/\",\"name\":\"U.S. Consulate Shanghai Air Quality Monitor\"},{\"url\":\"https://waqi.info/\",\"name\":\"World Air Quality Index Project\"}],\"city\":{\"geo\":[31.2047372,121.4489017],\"name\":\"Shanghai (上海)\",\"url\":\"https://aqicn.org/city/shanghai\"},\"dominentpol\":\"pm25\",\"iaqi\":{\"co\":{\"v\":6.4},\"h\":{\"v\":20.4},\"no2\":{\"v\":20.2},\"o3\":{\"v\":67.5},\"p\":{\"v\":1019.2},\"pm10\":{\"v\":57},\"pm25\":{\"v\":824},\"so2\":{\"v\":4.6},\"t\":{\"v\":17.5},\"w\":{\"v\":0.3}},\"time\":{\"s\":\"2019-04-01 17:00:00\",\"tz\":\"+08:00\",\"v\":1554138000},\"debug\":{\"sync\":\"2019-04-01T18:49:19+09:00\"}}}"

# how to install jq -> https://stedolan.github.io/jq/download/
AQI=$(echo "${DATA}" | /usr/local/bin/jq '.data.aqi' | sed -e "s/\"//g")

function colorize {
  if [ "$AQI" = "-" ]; then
    echo "${COLORS[6]}"
  elif [ "$1" -le 50 ]; then
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

COLOR="$(colorize "${AQI}")"
echo "😷${AQI} | color=${COLOR} ${MENUFONT}"

echo "---"
echo "Detail... | href=${URL}"
echo "Refresh... | refresh=true"