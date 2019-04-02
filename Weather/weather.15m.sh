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


WEATHER_DATA=$(curl -s "http://apis.juhe.cn/simpleWeather/query?city=${WEATHER_CITY}&key=${WEATHER_TOKEN}")
# DELETE ME, TEST DATA
WEATHER_DATA="{\"reason\":\"查询成功!\",\"result\":{\"city\":\"上海\",\"realtime\":{\"temperature\":\"15\",\"humidity\":\"25\",\"info\":\"晴\",\"wid\":\"00\",\"direct\":\"北风\",\"power\":\"0级\",\"aqi\":\"55\"},\"future\":[{\"date\":\"2019-04-01\",\"temperature\":\"9\\/17℃\",\"weather\":\"晴转多云\",\"wid\":{\"day\":\"00\",\"night\":\"01\"},\"direct\":\"南风\"},{\"date\":\"2019-04-02\",\"temperature\":\"11\\/16℃\",\"weather\":\"阴转多云\",\"wid\":{\"day\":\"02\",\"night\":\"01\"},\"direct\":\"东南风转东风\"},{\"date\":\"2019-04-03\",\"temperature\":\"11\\/17℃\",\"weather\":\"阴\",\"wid\":{\"day\":\"02\",\"night\":\"02\"},\"direct\":\"东风转东南风\"},{\"date\":\"2019-04-04\",\"temperature\":\"13\\/15℃\",\"weather\":\"小雨\",\"wid\":{\"day\":\"07\",\"night\":\"07\"},\"direct\":\"东南风转南风\"},{\"date\":\"2019-04-05\",\"temperature\":\"13\\/19℃\",\"weather\":\"多云\",\"wid\":{\"day\":\"01\",\"night\":\"01\"},\"direct\":\"西北风转南风\"}]},\"error_code\":0}"

WEATHER_RES_REALTIME=$(echo "${WEATHER_DATA}" | /usr/local/bin/jq '.result.realtime')
WEATHER_RES_REALTIME_INFO=$(echo "${WEATHER_RES_REALTIME}" | /usr/local/bin/jq -r '.info')
WEATHER_RES_REALTIME_TEMPERATURE=$(echo "${WEATHER_RES_REALTIME}" | /usr/local/bin/jq -r '.temperature')
WEATHER_FUTURE=$(echo "${WEATHER_DATA}" | /usr/local/bin/jq '.result.future')
WEATHER_FUTURE_LENGTH=$(echo "${WEATHER_FUTURE}" | /usr/local/bin/jq 'length')

AQI_DATA=$(curl -s "http://api.waqi.info/feed/${AQI_CITY}/?token=${AQI_TOKEN}")
# DELETE ME, TEST DATA
AQI_DATA="{\"status\":\"ok\",\"data\":{\"aqi\":824,\"idx\":1437,\"attributions\":[{\"url\":\"http://www.semc.gov.cn/\",\"name\":\"Shanghai Environment Monitoring Center(上海市环境监测中心)\"},{\"url\":\"http://106.37.208.233:20035/emcpublish/\",\"name\":\"China National Urban air quality real-time publishing platform (全国城市空气质量实时发布平台)\"},{\"url\":\"https://china.usembassy-china.org.cn/embassy-consulates/shanghai/air-quality-monitor-stateair/\",\"name\":\"U.S. Consulate Shanghai Air Quality Monitor\"},{\"url\":\"https://waqi.info/\",\"name\":\"World Air Quality Index Project\"}],\"city\":{\"geo\":[31.2047372,121.4489017],\"name\":\"Shanghai (上海)\",\"url\":\"https://aqicn.org/city/shanghai\"},\"dominentpol\":\"pm25\",\"iaqi\":{\"co\":{\"v\":6.4},\"h\":{\"v\":20.4},\"no2\":{\"v\":20.2},\"o3\":{\"v\":67.5},\"p\":{\"v\":1019.2},\"pm10\":{\"v\":57},\"pm25\":{\"v\":824},\"so2\":{\"v\":4.6},\"t\":{\"v\":17.5},\"w\":{\"v\":0.3}},\"time\":{\"s\":\"2019-04-01 17:00:00\",\"tz\":\"+08:00\",\"v\":1554138000},\"debug\":{\"sync\":\"2019-04-01T18:49:19+09:00\"}}}"

# how to install jq -> https://stedolan.github.io/jq/download/
AQI_RES=$(echo "${AQI_DATA}" | /usr/local/bin/jq '.data.aqi')

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

COLOR="$(aqi_colorize "${AQI_RES}")"
echo "🌡️${WEATHER_RES_REALTIME_INFO}${WEATHER_RES_REALTIME_TEMPERATURE}℃😷${AQI_RES} | color=${COLOR} ${MENUFONT}"
echo "---"
for(( i=0;i<WEATHER_FUTURE_LENGTH;i++)) do
  WEATHER_FUTURE_N=$(echo "${WEATHER_FUTURE}" | /usr/local/bin/jq ".[${i}]")
  WEATHER_FUTURE_N_DATE=$(echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.date')
  WEATHER_FUTURE_N_WEATHER=$(echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.weather')
  WEATHER_FUTURE_N_TEMPERATURE=$(echo "${WEATHER_FUTURE_N}" | /usr/local/bin/jq -r '.temperature')
  echo "${WEATHER_FUTURE_N_DATE} ${WEATHER_FUTURE_N_WEATHER}（${WEATHER_FUTURE_N_TEMPERATURE}）";
done;
echo "AQI Detail... | href=${AQI_DETAIL_URL}"
echo "Refresh... | refresh=true"