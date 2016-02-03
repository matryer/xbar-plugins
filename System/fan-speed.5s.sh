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

declare -a FANS # list available fans with smc -f
declare -a FAN_LABEL # list available fans with smc -f
FANS=("Fan #0" "Fan #1" "Fan #2")
# FAN_LABEL=("CPU" "ODD" "HDD") # Uncomment to add label, must 1-to-1 map the above array
FAN_SPEEDS="♨︎ " # Set your own prefix

for ((i = 0; i < ${#FANS[@]}; i++)) ; do
  FAN_SPEED=$(/usr/local/bin/smc -f | grep -A1 "${FANS[$i]}" | grep Actual | awk '{printf "%s\n", $4}')
  if [ "$FAN_SPEED" != "" ] ; then
    if [ ! -z "${FAN_LABEL+x}" ]; then
       # Add labels if FAN_LABEL is declared
       FAN_SPEEDS="$FAN_SPEEDS${FAN_LABEL[$i]}: "
    fi
    FAN_SPEEDS="$FAN_SPEEDS$FAN_SPEED rpm "
  fi
done
echo "$FAN_SPEEDS| size=12"
