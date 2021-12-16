#!/bin/bash

# Display and kill running processes
#
# by Dan Engel (im@dan-engel.fyi)
#

# metadata
# <xbar.title>PID Killer</xbar.title>
# <xbar.version>v1.10.1</xbar.version>
# <xbar.author>Dan Engel</xbar.author>
# <xbar.author.github>dengel29</xbar.author.github>
# <xbar.desc>Display and kill running processes</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/8292535/108005602-1651c500-7034-11eb-982f-edc41a9554db.png</xbar.image>

echo "Current PIDs"
echo "---"
echo "Click on an item to kill the process | font=Tahoma-Bold"

if [[ "$1" = 'kill' ]]; then 
  kill -9 "$2"
fi

list=`lsof -i | awk '{a[$1","$2];}END {for (i in a) print i;}'`;
IFS=$'\n' arr=($list)
IFS=','
echo "${#arr[@]} running currently | color=green font=Tahoma size=12"
for val in "${arr[@]}"; do
  item=($val)
  echo "${item[0]}:${item[1]} | font=AndaleMono bash=$0 param1=kill param2=${item[1]} terminal=false refresh=true"
done
