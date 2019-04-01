#!/bin/bash
#
# <bitbar.title>weather</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chongyu Yuan</bitbar.author>
# <bitbar.author.github>nnnggel</bitbar.author.github>
# <bitbar.desc>real-time Chinese weather info(includes aqi), required jq(https://aqicn.org/api/) and aqi token(https://aqicn.org/api/)(</bitbar.desc>
# <bitbar.image>https://s2.ax1x.com/2019/04/01/As7pVO.jpg</bitbar.image>
# <bitbar.dependencies>bash,jq</bitbar.dependencies>
# <bitbar.abouturl>http://www.yuanchongyu.com</bitbar.abouturl>

MENUFONT="size=12 font=UbuntuMono-Bold"
COLORS=("#0ed812" "#ffde33" "#ff9933" "#cc0033" "#660099" "#7e0023")

# where to get the token -> https://www.juhe.cn/docs/api/id/73
WEATHER_TOKEN="xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
WEATHER_CITY="xx" #eg. 上海

# where to get the token -> https://aqicn.org/api/
AQI_TOKEN="xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
AQI_CITY="xx" #eg. shanghai
AQI_DETAIL_URL="http://aqicn.org/city/${AQI_CITY}/"


WEATHER_DATA=`curl -s "http://apis.juhe.cn/simpleWeather/query?city=${WEATHER_CITY}&key=${WEATHER_TOKEN}"`
WEATHER_RES_REALTIME=`echo "${WEATHER_DATA}" | /usr/local/bin/jq '.result.realtime'`
WEATHER_RES_REALTIME_INFO=`echo ${WEATHER_RES_REALTIME} | /usr/local/bin/jq -r '.info'`
WEATHER_RES_REALTIME_TEMPERATURE=`echo ${WEATHER_RES_REALTIME} | /usr/local/bin/jq -r '.temperature'`
WEATHER_FUTURE=`echo "${WEATHER_DATA}" | /usr/local/bin/jq '.result.future'`
WEATHER_FUTURE_LENGTH=`echo "${WEATHER_FUTURE}" | /usr/local/bin/jq 'length'`

AQI_DATA=`curl -s "http://api.waqi.info/feed/${AQI_CITY}/?token=${AQI_TOKEN}"`
# how to install jq -> https://stedolan.github.io/jq/download/
AQI_RES=`echo "${AQI_DATA}" | /usr/local/bin/jq '.data.aqi'`

function aqi_colorize {
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

echo "🌡️${WEATHER_RES_REALTIME_INFO}${WEATHER_RES_REALTIME_TEMPERATURE}℃😷${AQI_RES} | color=$(aqi_colorize ${AQI_RES}) ${MENUFONT}"
echo "---"
for(( i=0;i<WEATHER_FUTURE_LENGTH;i++)) do
  WEATHER_FUTURE_N=`echo "${WEATHER_FUTURE}" | /usr/local/bin/jq ".[${i}]"`
  WEATHER_FUTURE_N_DATE=`echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.date'`
  WEATHER_FUTURE_N_WEATHER=`echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.weather'`
  WEATHER_FUTURE_N_TEMPERATURE=`echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.temperature'`
  echo "${WEATHER_FUTURE_N_DATE} ${WEATHER_FUTURE_N_WEATHER}（${WEATHER_FUTURE_N_TEMPERATURE}）";
done;
echo "AQI Detail... | href=${AQI_DETAIL_URL}"
echo "Refresh... | refresh=true"