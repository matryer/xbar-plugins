#!/bin/bash
#
# <xbar.title>process-monitoring</xbar.title>
# <xbar.version>v0.1.0</xbar.version>
# <xbar.author>Olivier Tille</xbar.author>
# <xbar.author.github>oliviernt</xbar.author.github>
# <xbar.desc>Monitors CPU and Memory usage for a certain process</xbar.desc>
# <xbar.image>http://i.imgur.com/ApLSN8L.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/oliviernt/bitbar-plugins/blob/monitoring/Dev/Monitoring/process-monitoring.1s.sh</xbar.abouturl>

PROCESS="BitBar.app"

# shellcheck disable=SC2009
id=$(ps aux | grep -i "${PROCESS}" | grep -v grep | awk '{print $2}')
echo -n "${PROCESS}: "
top -l 1 -pid "$id" -stats cpu,mem | tail -1 | awk '{print "CPU: " $1 " - MEM: " $2}'
echo "---"
top -l 1 -pid "$id"
