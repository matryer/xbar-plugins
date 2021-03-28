#!/bin/sh

# <xbar.title>Trackpad Battery</xbar.title>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.version>1.0</xbar.version>

PERCENTAGE=$(ioreg -n BNBTrackpadDevice | fgrep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Trackpad: $PERCENTAGE%"
fi
