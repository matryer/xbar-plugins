#!/bin/bash

PROCESS="BitBar.app"

id=`ps aux | grep -i "${PROCESS}" | grep -v grep | awk '{print $2}'`
echo -n "${PROCESS}: "
top -l 1 -pid $id -stats cpu,mem | tail -1 | awk '{print "CPU: " $1 " - MEM: " $2}'
echo "---"
top -l 1 -pid $id
