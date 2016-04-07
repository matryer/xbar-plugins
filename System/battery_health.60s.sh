#!/bin/bash
#
# <bitbar.title>Battery Health</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Andros Fenollosa</bitbar.author>
# <bitbar.author.github>tanrax</bitbar.author.github>
# <bitbar.image>https://programadorwebvalencia.com/wp-content/uploads/2016/04/Screen-Shot-2016-04-05-at-12.47.25.jpg</bitbar.image>
# <bitbar.desc>Shows power percentaje and notice when you load</bitbar.desc>

# Variables
BATTERY=$(ioreg -l | awk '$3~/Capacity/{c[$3]=$5}END{OFMT="%.0f%";max=c["\"MaxCapacity\""];print(max>0?100*c["\"CurrentCapacity\""]/max:"?")}')
TYPE=$(pmset -g cap | sed -ne 's/^Capabilities for \(.*\) Power:$/\1/p')
POR_LOW=45
POR_HIGH=85
LOW=False
HIGH=False
SAVE_LOCATION=$TMPDIR/batteryHealth
BAD='🔴'
AC='⚡️'

# Get data
if [ -f "$SAVE_LOCATION" ]; then
    DATA=$(cat "$SAVE_LOCATION")
else
    DATA="$LOW|$HIGH"
fi

LOW=$(echo "$DATA" | cut -d "|" -f1)
HIGH=$(echo "$DATA" | cut -d "|" -f2)

# Functions
function changeStatus {
    osascript -e "display notification \"$2\" with title \"$1\" sound name \"$3\"" &> /dev/null
}

function batteryLow {
	LOW=True
    changeStatus "Battery Low" "$BATTERY"% "Blow"
}

function batteryNormal {
	HIGH=False
	LOW=False
}

function batteryHigh {
	HIGH=True
    changeStatus "Battery high" "$BATTERY"% "Blow"
}

# Logic
if [ "$BATTERY" -le $POR_LOW ] && [ $LOW = False ]; then
	batteryLow
elif [ "$BATTERY" -ge $POR_HIGH ] && [ $HIGH = False ]; then
	batteryHigh
elif [ "$BATTERY" -le $POR_HIGH ] && [ "$BATTERY" -ge $POR_LOW ]; then
	batteryNormal
fi

# Save data
echo "$LOW|$HIGH" > "$SAVE_LOCATION";

# View battery
if [ "$TYPE" = "AC" ]; then
	echo $AC "$BATTERY"%
else
	if [ $HIGH = True ] || [ $LOW = True ]; then
			echo $BAD "$BATTERY"%
		else 
			echo "$BATTERY"%
	fi
fi
