#!/bin/bash
# <bitbar.title>Fan Speed</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Eric Ripa</bitbar.author>
# <bitbar.author.github>eripa</bitbar.author.github>
# <bitbar.desc>This plugin displays the current fan speed from SMC readings (requires external 'smc' binary)</bitbar.desc>
# <bitbar.dependencies>smc</bitbar.dependencies>
#
# 'smc' can be downloaded from: http://www.eidac.de/smcfancontrol/smcfancontrol_2_4.zip
# One-liner:
# curl -LO http://www.eidac.de/smcfancontrol/smcfancontrol_2_4.zip && unzip -d temp_dir_smc smcfancontrol_2_4.zip && cp temp_dir_smc/smcFanControl.app/Contents/Resources/smc /usr/local/bin/smc ; rm -rf temp_dir_smc smcfancontrol_2_4.zip

declare -i FANS # number of fans with smc -k FNum -r
declare -a FAN_LABEL # list available fans with smc -f
FANS=$(/usr/local/bin/smc -k FNum -r | awk '{ printf "%d\n", $4}')
# FAN_LABEL=("CPU" "ODD" "HDD") # Uncomment to add label, must 1-to-1 map the above array
FAN_SPEEDS="♨︎ " # Set your own prefix

for ((i = 0; i < FANS; i++)) ; do
  FAN_SPEED=$(/usr/local/bin/smc -k F${i}Ac -r | awk '{ printf "%s\n", $3}' | grep '^[0-9]*[.][0-9]*$' | awk '{ printf "%d\n", $1}')
  if [ "$FAN_SPEED" != "" ] ; then
    if [ -n "${FAN_LABEL+x}" ]; then
       # Add labels if FAN_LABEL is declared
       FAN_SPEEDS="$FAN_SPEEDS${FAN_LABEL[$i]}: "
    fi
    FAN_SPEEDS="$FAN_SPEEDS$FAN_SPEED rpm "
  fi
done
echo "$FAN_SPEEDS| size=12"
