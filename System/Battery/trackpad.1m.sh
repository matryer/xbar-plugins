#!/bin/sh

PERCENTAGE=`ioreg -n BNBTrackpadDevice | fgrep BatteryPercent |fgrep -v { | sed 's/[^[:digit:]]//g'`

if [ "$PERCENTAGE" ]; then
        echo "Trackpad: $PERCENTAGE%"
fi

