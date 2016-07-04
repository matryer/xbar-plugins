#!/bin/sh

# <bitbar.title>Keybard Battery</bitbar.title>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>

PERCENTAGE=$(ioreg -c AppleBluetoothHIDKeyboard | grep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Keyboard: $PERCENTAGE%"
#else
#        echo "without bluetooth keyboard?"
fi

