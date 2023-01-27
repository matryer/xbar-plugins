#!/bin/bash

# <xbar.title>Switch Timezones</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Vasily Pleshakov</xbar.author>
# <xbar.author.github>wasapl</xbar.author.github>
# <xbar.desc>Allows to switch timezone for current time shown in the Bar. </xbar.desc>
# <xbar.image>https://i.imgur.com/0Oevp2W.png</xbar.image>
# <xbar.dependencies>BASH</xbar.dependencies>
SCRIPT_NAME=$(basename "$0")
SCRIPT_DIR=$(dirname "$0")
declare -A TZS
TZS["Moscow"]="Europe/Moscow"
TZS["London"]="Europe/London"
TZS["NewYork"]="US/Eastern"
TZS["SanFrancisco"]="US/Pacific"
TZS["UTC"]="UTC"

TEMP_FILE="$TMPDIR/${SCRIPT_NAME%%.*}.tmp"
if [ -s "${TEMP_FILE}" ]; then
    CUR_CITY=$(cat "${TEMP_FILE}")
else
    CUR_CITY="UTC"
fi

function get_time() {
    TZ=":${1}" date +'%m/%d %H:%M'
}

function menu() {
    cur_time=$(get_time "${TZS[$CUR_CITY]}"); echo "${CUR_CITY} ${cur_time}"
    echo "---"
    for city in "${!TZS[@]}"; do
            cur_time=$(get_time "${TZS[$city]}"); echo "$city ${cur_time} | bash='${SCRIPT_DIR}/${SCRIPT_NAME}' param1=chcity param2=$city terminal=false refresh=true"
    done
    echo "---"
    cur_time=$(date +%s); echo "Epoch $cur_time | bash='${SCRIPT_DIR}/${SCRIPT_NAME}' param1=copy param2=\"$cur_time\" terminal=false"
    cur_time=$(date +'%Y%m%d'); echo "YMD $cur_time | bash='${SCRIPT_DIR}/${SCRIPT_NAME}' param1=copy param2=\"$cur_time\" terminal=false"
    cur_time=$(date -u +"%Y-%m-%dT%H:%M:%SZ"); echo "ISO 8601 $cur_time | bash='${SCRIPT_DIR}/${SCRIPT_NAME}' param1=copy param2=\"$cur_time\" terminal=false"
}

if [[ "$#" -ge 1 ]];then
    if [[ "$1" == 'copy' ]] ; then
        echo -n "$2" | pbcopy
        echo COPIED "$2"
    fi
    if [[ "$1" == 'chcity' ]] ; then
        echo "$2" > "${TEMP_FILE}"
    fi
fi

menu
