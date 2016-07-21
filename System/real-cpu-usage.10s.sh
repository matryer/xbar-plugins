#!/bin/bash

# <bitbar.title>Real CPU Usage</bitbar.title>
# <bitbar.author>Mat Ryer and Tyler Bunnell</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Calcualtes and displays real CPU usage stats.</bitbar.desc>
# <bitbar.version>1.0</bitbar.version>

IDLE=$(top -F -R -l3 | grep "CPU usage" | tail -1 | egrep -o '[0-9]{0,3}\.[0-9]{0,2}% idle' | sed 's/% idle//')

USED=$(echo 100 - "$IDLE" | bc)

echo "CPU: $USED%"
