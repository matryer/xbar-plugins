#!/bin/bash
#
# <xbar.title>Air Quality Index</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Chongyu Yuan, Nick Xiao</xbar.author>
# <xbar.author.github>nnnggel, nicoster</xbar.author.github>
# <xbar.desc>Real-time Air Quality Index. </xbar.desc>
# <xbar.image>https://github.com/nicoster/assets/blob/fe2a768dc41ad7e4b164260346b5f0b461b8ba87/xbar-aqi.png?raw=true</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>http://www.yuanchongyu.com</xbar.abouturl>
# <xbar.var>string(URL="https://aqicn.org/city/usa/newyork/"): Navigate to `https://aqicn.org/`, find your city or nearest location, then copy from the address bar of the browser and paste it below.</xbar.var>


# URL examples
# URL="https://aqicn.org/city/usa/newyork/queens-college"
# URL="https://aqicn.org/city/california/santa-clara/san-jose-jackson-st/"
URL="https://aqicn.org/city/usa/newyork/?demo"

MENUFONT="size=12 font=UbuntuMono-Bold"
COLORS=("#0ed812" "#ffde33" "#ff9933" "#cc0033" "#660099" "#7e0023" "#404040")
EMOJIS=("😀" "🙁" "😨" "😷" "🤢" "💀" "☠️")

TYPES=("pm25" "pm10" "o3" "no2" "so2" "co" "t" "p" "h" "w")
TITLES=("[PM25]" "[PM10]" "[  O3]" "[ NO2]" "[ SO2]" "[  CO]" "[TEMP]" "[PRES]" "[HUMI]" "[WIND]")

function extract_by_id {
  echo "$1" | sed -n -E "s/^.*id='$2'[^>]*>(<[^>]+>)?([^<]*).*$/\2/p"
}

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

HTML=$(curl -s $URL)
IMGS=$(echo "${HTML}" | grep -o -E "<img class='aqi-graph-img[^:]+:image/png;base64,[^']*" | sed -n -E "s/^.*base64,([^']*).*$/\1/p")
AQI=$(extract_by_id "${HTML}" "aqiwgtvalue")

COLOR="$(colorize "${AQI}")"
EMOJI="$(emoji "${AQI}")"
echo "${EMOJI} ${AQI} | color=${COLOR} ${MENUFONT}"

echo "---"
echo "Sync with ${URL} | refresh=true"
if [[ ${URL} =~ .*"demo" ]]; then
  echo "Press ⌘ E to configure your location"
fi

echo "─────────────────────────────────────── min-cur-max | font=Monaco size=12"

i=0
for img in ${IMGS} ;do
  MIN=$(extract_by_id "${HTML}" "min_${TYPES[$i]}")
  while [[ "${MIN}" == "" ]]; do
    i=$((i+1))
    MIN=$(extract_by_id "${HTML}" "min_${TYPES[$i]}")
  done

  printf "%s\t" "${TITLES[$i]}" 
  echo "${MIN}-$(extract_by_id "${HTML}" "cur_${TYPES[$i]}")-$(extract_by_id "${HTML}" "max_${TYPES[$i]}") | href=${URL} font=Monaco size=12 image=${img} " #
  i=$((i+1))
done

echo "Visit World Air Quality Index site to support the team | href=${URL} color=blue"
