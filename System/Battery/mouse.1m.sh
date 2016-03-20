#!/bin/sh
# <bitbar.title>Mouse battery</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Show battery percentage for Bluetooth Mouse</bitbar.desc>
# <bitbar.image>http://i.imgur.com/IqjZMJg.png</bitbar.image>

# works fine with Magic Mouse

PERCENTAGE=$(ioreg -n BNBMouseDevice | fgrep BatteryPercent | fgrep -v \{ | sed 's/[^[:digit:]]//g')

if [ "$PERCENTAGE" ]; then
        echo "Mouse: $PERCENTAGE%"
#else
#        echo "without bluetooth mouse?"
fi

