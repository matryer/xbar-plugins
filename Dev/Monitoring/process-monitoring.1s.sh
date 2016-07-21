#!/bin/bash
#
# <bitbar.title>process-monitoring</bitbar.title>
# <bitbar.version>v0.1.0</bitbar.version>
# <bitbar.author>Olivier Tille</bitbar.author>
# <bitbar.author.github>oliviernt</bitbar.author.github>
# <bitbar.desc>Monitors CPU and Memory usage for a certain process</bitbar.desc>
# <bitbar.image>http://i.imgur.com/ApLSN8L.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/oliviernt/bitbar-plugins/blob/monitoring/Dev/Monitoring/process-monitoring.1s.sh</bitbar.abouturl>

PROCESS="BitBar.app"

# shellcheck disable=SC2009
id=$(ps aux | grep -i "${PROCESS}" | grep -v grep | awk '{print $2}')
echo -n "${PROCESS}: "
top -l 1 -pid "$id" -stats cpu,mem | tail -1 | awk '{print "CPU: " $1 " - MEM: " $2}'
echo "---"
top -l 1 -pid "$id"
