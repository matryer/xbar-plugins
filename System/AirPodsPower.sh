#!/bin/bash

# <xbar.title>AirPods Power</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gonzalo Serrano Revuelta</xbar.author>
# <xbar.author.github>gonzaloserrano</xbar.author.github>
# <xbar.desc>Displays AirPods battery</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>

# Based on AirPods Battery CLI, Version 2.3 - https://github.com/duk242/AirPodsBatteryCLI

OUTPUT='🎧'
SYSTEM_PROFILER=$(system_profiler SPBluetoothDataType 2>/dev/null)

# Find connected AirPods (look for headphones in the Connected section)
# We need to get the device block that contains "Minor Type: Headphones"
CONNECTED_SECTION=$(awk '/Connected:/,/Not Connected:/' <<<"${SYSTEM_PROFILER}")
# Find the device block containing headphones - get lines from device name (with indent) backwards until we find battery info
AIRPODS_BLOCK=$(echo "${CONNECTED_SECTION}" | awk '
    /^          [A-Za-z].*:$/ { device=""; in_device=1 }
    in_device { device = device "\n" $0 }
    /Minor Type: Headphones/ && in_device {
        print device
        exit
    }
')

if [[ -n "${AIRPODS_BLOCK}" ]]; then
    # Extract battery levels from system_profiler output
    # Note: Case battery only appears when case is open or was recently opened
    LEFT=$(echo "${AIRPODS_BLOCK}" | awk '/Left Battery Level:/{gsub(/%/,""); print $NF}')
    RIGHT=$(echo "${AIRPODS_BLOCK}" | awk '/Right Battery Level:/{gsub(/%/,""); print $NF}')
    CASE=$(echo "${AIRPODS_BLOCK}" | awk '/Case Battery Level:/{gsub(/%/,""); print $NF}')
    COMBINED=$(echo "${AIRPODS_BLOCK}" | awk '/Battery Level:/ && !/Left/ && !/Right/ && !/Case/{gsub(/%/,""); print $NF}')

    # Build output string
    [[ -n "${LEFT}" ]] && OUTPUT="${OUTPUT} L${LEFT}"
    [[ -n "${RIGHT}" ]] && OUTPUT="${OUTPUT} R${RIGHT}"
    [[ -n "${CASE}" ]] && OUTPUT="${OUTPUT} C${CASE}"
    [[ -n "${COMBINED}" && -z "${LEFT}" && -z "${RIGHT}" ]] && OUTPUT="${OUTPUT} ${COMBINED}"

    echo "${OUTPUT}"
else
    printf "%s\n---\nNot connected" "${OUTPUT}"
fi
