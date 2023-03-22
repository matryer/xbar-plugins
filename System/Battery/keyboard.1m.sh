#!/bin/bash

# <xbar.title>Keybard Battery</xbar.title>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>

PERCENTAGE=$(ioreg -c AppleBluetoothHIDKeyboard | grep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')
# Detect and adjust for M1 Mac
if [[ $(uname -m) == 'arm64' ]]; then
  PERCENTAGE=$(ioreg -c AppleDeviceManagementHIDEventService -r -l | grep -i keyboard -A 20  | grep BatteryPercent | cut -d = -f2 | cut -d ' ' -f2)
fi

if [ "$PERCENTAGE" ]; then
        echo "Keyboard: $PERCENTAGE%"
#else
#        echo "without bluetooth keyboard?"
fi

