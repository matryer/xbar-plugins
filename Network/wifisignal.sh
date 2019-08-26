#!/bin/bash

# <bitbar.title>Active WIFI Signal</bitbar.title>
# <bitbar.author>Bryan Stone</bitbar.author>
# <bitbar.author.github>aegixx</bitbar.author.github>
# <bitbar.desc>Displays currently connected WIFI Signal</bitbar.desc>

# Themes copied from here: http://colorbrewer2.org/
# shellcheck disable=SC2034
RED_GREEN_THEME=("#d73027" "#fc8d59" "#fee08b" "#d9ef8b" "#91cf60" "#1a9850")
COLORS=("${RED_GREEN_THEME[@]}")

WIFIDATA=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I)
SSID=$(echo "$WIFIDATA" | awk '/ SSID/ {print substr($0, index($0, $2))}')
SIGNAL=$(echo "$WIFIDATA" | awk '/ agrCtlRSSI/ {print substr($0, index($0, $2))}')
NOISE=$(echo "$WIFIDATA" | awk '/ agrCtlNoise/ {print substr($0, index($0, $2))}')

SNR="$((SIGNAL - NOISE))"

# Signal Strength – 0dBm (strongest) and --100dBm (weakest). 
## -30 dBm  Amazing
## -50 dBm	Excellent
## -60 dBm	Good
## -67 dBm	Reliable
## -70 dBm	Okay
## -80 dBm	Not Good
## -90 dBm	Unusable
if (("$SIGNAL" >= -30)); then
    RATING="Amazing"
    COLOR=${COLORS[6]}
elif (("$SIGNAL" >= -50)); then
    RATING="Excellent"
    COLOR=${COLORS[5]}
elif (("$SIGNAL" >= -60)); then
    RATING="Good"
    COLOR=${COLORS[4]}
elif (("$SIGNAL" >= -67)); then
    RATING="Reliable"
    COLOR=${COLORS[3]}
elif (("$SIGNAL" >= -70)); then
    RATING="Okay"
    COLOR=${COLORS[2]}
elif (("$SIGNAL" >= -80)); then
    RATING="Not Good"
    COLOR=${COLORS[1]}
elif (("$SIGNAL" >= -90)); then
    RATING="Unusable"
    COLOR=${COLORS[0]}
else
    RATING="Unknown"
    COLOR="#ccc"
fi

# Signal Quality - quality ~= 2* (dBm + 100)
## High quality: 90% ~= -55dBm
## Medium quality: 50% ~= -75dBm
## Low quality: 30% ~= -85dBm
## Unusable quality: 8% ~= -96dBm
QUALITY="$((2 * SNR))"
QUALITY="$((QUALITY < 100 ? QUALITY : 100))"

echo "📶 $SSID | color=$COLOR"
echo "---"
echo "Signal: $SIGNAL dbM ($RATING)"
echo "Quality: $QUALITY% ($SNR dBm SNR)"
echo "---"
echo "Refresh... | refresh=true"
