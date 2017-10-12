#!/bin/bash

# <bitbar.title>ping</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Trung Đinh Quang, Grant Sherrick and Kent Karlsson</bitbar.author>
# <bitbar.author.github>thealmightygrant</bitbar.author.github>
# <bitbar.desc>Sends pings to a range of sites to determine network latency</bitbar.desc>
# <bitbar.image>http://i.imgur.com/lk3iGat.png?1</bitbar.image>
# <bitbar.dependencies>ping</bitbar.dependencies>

# This is a plugin of Bitbar
# https://github.com/matryer/bitbar
# It shows current ping to some servers at the top Menubar
# This helps me to know my current connection speed
#
# Authors: (Trung Đinh Quang) trungdq88@gmail.com and (Grant Sherrick) https://github.com/thealmightygrant

# Themes copied from here: http://colorbrewer2.org/
# shellcheck disable=SC2034
PURPLE_GREEN_THEME=("#762a83" "#9970ab" "#c2a5cf" "#a6dba0" "#5aae61" "#1b7837")
# shellcheck disable=SC2034
RED_GREEN_THEME=("#d73027" "#fc8d59" "#fee08b" "#d9ef8b" "#91cf60" "#1a9850")
# shellcheck disable=SC2034
ORIGINAL_THEME=("#acacac" "#ff0101" "#cc673b" "#ce8458" "#6bbb15" "#0ed812")

# Configuration
COLORS=(${RED_GREEN_THEME[@]})
MENUFONT="" #size=10 font=UbuntuMono-Bold"
FONT=""
MAX_PING=1000
SITES=(google.com youtube.com wikipedia.org github.com)

#grab ping times for all sites
SITE_INDEX=0
PING_TIMES=

while [ $SITE_INDEX -lt ${#SITES[@]} ]; do
    NEXT_SITE="${SITES[$SITE_INDEX]}"
    if RES=$(ping -c 2 -n -q "$NEXT_SITE" 2>/dev/null); then
        NEXT_PING_TIME=$(echo "$RES" | awk -F '/' 'END {printf "%.0f\n", $5}')
    else
        NEXT_PING_TIME=$MAX_PING
    fi

    if [ -z "$PING_TIMES" ]; then
        PING_TIMES=($NEXT_PING_TIME)
    else
        PING_TIMES=(${PING_TIMES[@]} $NEXT_PING_TIME)
    fi
    SITE_INDEX=$(( SITE_INDEX + 1 ))
done

# Calculate the average ping
SITE_INDEX=0
AVG=0
while [ $SITE_INDEX -lt ${#SITES[@]} ]; do
    AVG=$(( (AVG + ${PING_TIMES[$SITE_INDEX]}) ))
    SITE_INDEX=$(( SITE_INDEX + 1 ))
done
AVG=$(( AVG / ${#SITES[@]} ))

# Calculate STD dev
SITE_INDEX=0
AVG_DEVS=0
while [ $SITE_INDEX -lt ${#SITES[@]} ]; do
    AVG_DEVS=$(( AVG_DEVS + (${PING_TIMES[$SITE_INDEX]} - AVG)**2 ))
    SITE_INDEX=$(( SITE_INDEX + 1 ))
done
AVG_DEVS=$(( AVG_DEVS / ${#SITES[@]} ))
SD=$(echo "sqrt ( $AVG_DEVS )" | bc -l | awk '{printf "%d\n", $1}')

if [ $AVG -ge $MAX_PING ]; then
  MSG=" ☠ "
else
  MSG='⚡'"$AVG"'±'"$SD"
fi

function colorize {
  if [ "$1" -ge $MAX_PING ]; then
    echo "${COLORS[0]}"
  elif [ "$1" -ge 600 ]; then
    echo "${COLORS[1]}"
  elif [ "$1" -ge 300 ]; then
    echo "${COLORS[2]}"
  elif [ "$1" -ge 100 ]; then
    echo "${COLORS[3]}"
  elif [ "$1" -ge 50 ]; then
    echo "${COLORS[4]}"
  else
    echo "${COLORS[5]}"
  fi
}

echo "$MSG | color=$(colorize $AVG) $MENUFONT"
echo "---"
SITE_INDEX=0
while [ $SITE_INDEX -lt ${#SITES[@]} ]; do
    PING_TIME=${PING_TIMES[$SITE_INDEX]}
    if [ $PING_TIME -eq $MAX_PING ]; then
        PING_TIME="☠"
    else
        PING_TIME="$PING_TIME ms | color=$(colorize $PING_TIME) $FONT"
    fi

    echo "${SITES[$SITE_INDEX]}: $PING_TIME"
    SITE_INDEX=$(( SITE_INDEX + 1 ))
done

echo "---"
echo "Refresh... | refresh=true"
