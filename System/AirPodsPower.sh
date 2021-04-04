#!/bin/bash

# <xbar.title>AirPods Power</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gonzalo Serrano Revuelta</xbar.author>
# <xbar.author.github>gonzaloserrano</xbar.author.github>
# <xbar.desc>Displays AirPods battery</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>

# Based on AirPods Battery CLI, Version 2.3 - https://github.com/duk242/AirPodsBatteryCLI

OUTPUT='🎧'
BLUETOOTH_DEFAULTS=$(defaults read /Library/Preferences/com.apple.Bluetooth)
SYSTEM_PROFILER=$(system_profiler SPBluetoothDataType 2>/dev/null)
MAC_ADDR=$(grep -b2 "Minor Type: Headphones"<<<"${SYSTEM_PROFILER}"|awk '/Address/{print $3}')
CONNECTED=$(grep -ia6 "${MAC_ADDR}"<<<"${SYSTEM_PROFILER}"|awk '/Connected: Yes/{print 1}')
BLUETOOTH_DATA=$(grep -ia6 '"'"${MAC_ADDR}"'"'<<<"${BLUETOOTH_DEFAULTS}")
BATTERY_LEVELS=("BatteryPercentCombined" "HeadsetBattery" "BatteryPercentSingle" "BatteryPercentCase" "BatteryPercentLeft" "BatteryPercentRight")

if [[ "${CONNECTED}" ]]; then
    for I in "${BATTERY_LEVELS[@]}"; do
        declare -x "${I}"="$(awk -v pat="${I}" '$0~pat{gsub (";",""); print $3 }'<<<"${BLUETOOTH_DATA}")"
        [[ -n "${!I}" ]] && OUTPUT="${OUTPUT} $(awk '/BatteryPercent/{print substr($0,15,1)}'<<<"${I}")${!I}"
    done
    echo "${OUTPUT}"
else
    printf "%s\n---\nNot connected" "${OUTPUT}"
fi
