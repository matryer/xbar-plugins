#!/bin/bash
#
# <xbar.title>weather</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Chongyu Yuan</xbar.author>
# <xbar.author.github>nnnggel</xbar.author.github>
# <xbar.desc>real-time Chinese weather info(includes aqi), required jq(https://aqicn.org/api/) and aqi token(https://aqicn.org/api/)(</xbar.desc>
# <xbar.image>/9j/4AAQSkZJRgABAQAAkACQAAD/4QB0RXhpZgAATU0AKgAAAAgABAEaAAUAAAABAAAAPgEbAAUAAAABAAAARgEoAAMAAAABAAIAAIdpAAQAAAABAAAATgAAAAAAAACQAAAAAQAAAJAAAAABAAKgAgAEAAAAAQAAANigAwAEAAAAAQAAACgAAAAA/+0AOFBob3Rvc2hvcCAzLjAAOEJJTQQEAAAAAAAAOEJJTQQlAAAAAAAQ1B2M2Y8AsgTpgAmY7PhCfv/iB+hJQ0NfUFJPRklMRQABAQAAB9hhcHBsAiAAAG1udHJSR0IgWFlaIAfZAAIAGQALABoAC2Fjc3BBUFBMAAAAAGFwcGwAAAAAAAAAAAAAAAAAAAAAAAD21gABAAAAANMtYXBwbAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC2Rlc2MAAAEIAAAAb2RzY20AAAF4AAAFnGNwcnQAAAcUAAAAOHd0cHQAAAdMAAAAFHJYWVoAAAdgAAAAFGdYWVoAAAd0AAAAFGJYWVoAAAeIAAAAFHJUUkMAAAecAAAADmNoYWQAAAesAAAALGJUUkMAAAecAAAADmdUUkMAAAecAAAADmRlc2MAAAAAAAAAFEdlbmVyaWMgUkdCIFByb2ZpbGUAAAAAAAAAAAAAABRHZW5lcmljIFJHQiBQcm9maWxlAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABtbHVjAAAAAAAAAB8AAAAMc2tTSwAAACgAAAGEZGFESwAAAC4AAAGsY2FFUwAAACQAAAHadmlWTgAAACQAAAH+cHRCUgAAACYAAAIidWtVQQAAACoAAAJIZnJGVQAAACgAAAJyaHVIVQAAACgAAAKaemhUVwAAABYAAALCbmJOTwAAACYAAALYY3NDWgAAACIAAAL+aGVJTAAAAB4AAAMgaXRJVAAAACgAAAM+cm9STwAAACQAAANmZGVERQAAACwAAAOKa29LUgAAABYAAAO2c3ZTRQAAACYAAALYemhDTgAAABYAAAPMamFKUAAAABoAAAPiZWxHUgAAACIAAAP8cHRQTwAAACYAAAQebmxOTAAAACgAAAREZXNFUwAAACYAAAQedGhUSAAAACQAAARsdHJUUgAAACIAAASQZmlGSQAAACgAAASyaHJIUgAAACgAAATacGxQTAAAACwAAAUCcnVSVQAAACIAAAUuYXJFRwAAACYAAAVQZW5VUwAAACYAAAV2AFYBYQBlAG8AYgBlAGMAbgD9ACAAUgBHAEIAIABwAHIAbwBmAGkAbABHAGUAbgBlAHIAZQBsACAAUgBHAEIALQBiAGUAcwBrAHIAaQB2AGUAbABzAGUAUABlAHIAZgBpAGwAIABSAEcAQgAgAGcAZQBuAOgAcgBpAGMAQx6lAHUAIABoAOwAbgBoACAAUgBHAEIAIABDAGgAdQBuAGcAUABlAHIAZgBpAGwAIABSAEcAQgAgAEcAZQBuAOkAcgBpAGMAbwQXBDAEMwQwBDsETAQ9BDgEOQAgBD8EQAQ+BEQEMAQ5BDsAIABSAEcAQgBQAHIAbwBmAGkAbAAgAGcA6QBuAOkAcgBpAHEAdQBlACAAUgBWAEIAwQBsAHQAYQBsAOEAbgBvAHMAIABSAEcAQgAgAHAAcgBvAGYAaQBskBp1KAAgAFIARwBCACCCcl9pY8+P8ABHAGUAbgBlAHIAaQBzAGsAIABSAEcAQgAtAHAAcgBvAGYAaQBsAE8AYgBlAGMAbgD9ACAAUgBHAEIAIABwAHIAbwBmAGkAbAXkBegF1QXkBdkF3AAgAFIARwBCACAF2wXcBdwF2QBQAHIAbwBmAGkAbABvACAAUgBHAEIAIABnAGUAbgBlAHIAaQBjAG8AUAByAG8AZgBpAGwAIABSAEcAQgAgAGcAZQBuAGUAcgBpAGMAQQBsAGwAZwBlAG0AZQBpAG4AZQBzACAAUgBHAEIALQBQAHIAbwBmAGkAbMd8vBgAIABSAEcAQgAg1QS4XNMMx3xmbpAaACAAUgBHAEIAIGPPj/Blh072TgCCLAAgAFIARwBCACAw1zDtMNUwoTCkMOsDkwO1A70DuQO6A8wAIAPAA8EDvwPGA68DuwAgAFIARwBCAFAAZQByAGYAaQBsACAAUgBHAEIAIABnAGUAbgDpAHIAaQBjAG8AQQBsAGcAZQBtAGUAZQBuACAAUgBHAEIALQBwAHIAbwBmAGkAZQBsDkIOGw4jDkQOHw4lDkwAIABSAEcAQgAgDhcOMQ5IDicORA4bAEcAZQBuAGUAbAAgAFIARwBCACAAUAByAG8AZgBpAGwAaQBZAGwAZQBp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</xbar.image>
# <xbar.dependencies>bash,jq</xbar.dependencies>
# <xbar.abouturl>http://www.yuanchongyu.com</xbar.abouturl>

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