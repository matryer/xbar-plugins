#!/bin/bash
#
# <xbar.title>Air Quality Index</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Chongyu Yuan</xbar.author>
# <xbar.author.github>nnnggel</xbar.author.github>
# <xbar.desc>Real-time Air Quality Index. You need to install the `jq` package, then set the API_TOKEN and CITY in this plugin.</xbar.desc>
# <xbar.image>https://i.imgur.com/7bc5qqh.jpg</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>http://www.yuanchongyu.com</xbar.abouturl>
# <xbar.var>string(JQ_PATH="/usr/local/bin/jq"): Path to jq JSON processor. See https://stedolan.github.io/jq/download/</xbar.var>
# <xbar.var>string(API_TOKEN=""): API Token from https://aqicn.org/api/</xbar.var>
# <xbar.var>string(CITY="shanghai"): Major city or location URL path after `https://aqicn.org/city/`, excluding trailing slash if one exists.</xbar.var>

# how to install jq -> https://stedolan.github.io/jq/download/
# homebrew example: `$ brew install jq`

MENUFONT="size=12 font=UbuntuMono-Bold"
COLORS=("#0ed812" "#ffde33" "#ff9933" "#cc0033" "#660099" "#7e0023" "#404040")
EMOJIS=("😀" "🙁" "😨" "😷" "🤢" "💀" "☠️")
URL="https://aqicn.org/city/${CITY}"
API_URL="https://api.waqi.info/feed/${CITY}/?token=${API_TOKEN}"

DATA=$(curl -s ${API_URL})

# DELETE ME, TEST DATA
# DATA="{\"status\":\"ok\",\"data\":{\"aqi\":824,\"idx\":1437,\"attributions\":[{\"url\":\"http://www.semc.gov.cn/\",\"name\":\"Shanghai Environment Monitoring Center(上海市环境监测中心)\"},{\"url\":\"http://106.37.208.233:20035/emcpublish/\",\"name\":\"China National Urban air quality real-time publishing platform (全国城市空气质量实时发布平台)\"},{\"url\":\"https://china.usembassy-china.org.cn/embassy-consulates/shanghai/air-quality-monitor-stateair/\",\"name\":\"U.S. Consulate Shanghai Air Quality Monitor\"},{\"url\":\"https://waqi.info/\",\"name\":\"World Air Quality Index Project\"}],\"city\":{\"geo\":[31.2047372,121.4489017],\"name\":\"Shanghai (上海)\",\"url\":\"https://aqicn.org/city/shanghai\"},\"dominentpol\":\"pm25\",\"iaqi\":{\"co\":{\"v\":6.4},\"h\":{\"v\":20.4},\"no2\":{\"v\":20.2},\"o3\":{\"v\":67.5},\"p\":{\"v\":1019.2},\"pm10\":{\"v\":57},\"pm25\":{\"v\":824},\"so2\":{\"v\":4.6},\"t\":{\"v\":17.5},\"w\":{\"v\":0.3}},\"time\":{\"s\":\"2019-04-01 17:00:00\",\"tz\":\"+08:00\",\"v\":1554138000},\"debug\":{\"sync\":\"2019-04-01T18:49:19+09:00\"}}}"

# how to install jq -> https://stedolan.github.io/jq/download/
AQI=$(echo "${DATA}" | ${JQ_PATH} '.data.aqi' | sed -e "s/\"//g")

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

function emoji {
  if [ "$AQI" = "-" ]; then
    echo "${EMOJIS[6]}"
  elif [ "$1" -le 50 ]; then
    echo "${EMOJIS[0]}"
  elif [ "$1" -le 100 ]; then
    echo "${EMOJIS[1]}"
  elif [ "$1" -le 150 ]; then
    echo "${EMOJIS[2]}"
  elif [ "$1" -le 200 ]; then
    echo "${EMOJIS[3]}"
  elif [ "$1" -le 300 ]; then
    echo "${EMOJIS[4]}"
  else
    echo "${EMOJIS[5]}"
  fi
}

COLOR="$(colorize "${AQI}")"
EMOJI="$(emoji "${AQI}")"
echo "${EMOJI}${AQI} | color=${COLOR} ${MENUFONT}"

echo "---"
echo "Detail... | href=${URL}"
echo "Refresh... | refresh=true"
