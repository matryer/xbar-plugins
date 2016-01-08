#!/bin/sh

PERCENTAGE=`ioreg -c BNBTrackpadDevice | grep BatteryPercent | fgrep -v { | sed 's/[^[:digit:]]//g'`

if [ "$PERCENTAGE" ]; then
        echo "Trackpad: $PERCENTAGE%"
else
        echo "without bluetooth trackpad?"
fi
