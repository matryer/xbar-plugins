#!/bin/sh

# <bitbar.title>Trackpad Battery</bitbar.title>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>

PERCENTAGE=$(ioreg -n BNBTrackpadDevice | fgrep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Trackpad: $PERCENTAGE%"
fi

