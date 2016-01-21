#!/bin/bash

# uptime
# BitBar plugin
#
# by Mat Ryer, Matteo Ferrando
#
# Shows details about the current uptime of the system.

INFO=`uptime`
echo $INFO | tr "," " " | awk '{print "[ up "$3" days, "$5" ]"}'
echo "---"
echo $INFO | tr "," "\n" | tail -n 2
