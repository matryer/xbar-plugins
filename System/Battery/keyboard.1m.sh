#!/bin/sh

# <xbar.title>Keybard Battery</xbar.title>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>

PERCENTAGE=$(ioreg -c AppleBluetoothHIDKeyboard | grep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Keyboard: $PERCENTAGE%"
#else
#        echo "without bluetooth keyboard?"
fi

