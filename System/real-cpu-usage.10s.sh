#!/bin/bash

# <xbar.title>Real CPU Usage</xbar.title>
# <xbar.author>Mat Ryer and Tyler Bunnell</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Calcualtes and displays real CPU usage stats.</xbar.desc>
# <xbar.version>1.0</xbar.version>

if [ "$1" == "activitymonitor" ]; then
	open -a "Activity Monitor"
	exit
fi

IDLE=$(top -F -R -l3 | grep "CPU usage" | tail -1 | egrep -o '[0-9]{0,3}\.[0-9]{0,2}% idle' | sed 's/% idle//')

USED=$(echo 100 - "$IDLE" | bc)

echo "CPU: $USED%"
echo "---"
echo "Open Activity Monitor| bash='$0' param1=activitymonitor terminal=false"
