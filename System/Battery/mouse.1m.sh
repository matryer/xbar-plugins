#!/bin/sh
# <xbar.title>Mouse battery</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Alexandre Espinosa Menor</xbar.author>
# <xbar.author.github>alexandregz</xbar.author.github>
# <xbar.desc>Show battery percentage for Bluetooth Mouse</xbar.desc>
# <xbar.image>http://i.imgur.com/IqjZMJg.png</xbar.image>

# works fine with Magic Mouse

PERCENTAGE=$(ioreg -n BNBMouseDevice | fgrep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Mouse: $PERCENTAGE%"
#else
#        echo "without bluetooth mouse?"
fi

