#!/usr/bin/env zsh

# <bitbar.title>Switch Timezones</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Vasily Pleshakov</bitbar.author>
# <bitbar.author.github>wasapl</bitbar.author.github>
# <bitbar.desc>Allows to switch timezone for current time shown in the Bar. </bitbar.desc>
# <bitbar.image>https://i.imgur.com/0Oevp2W.png</bitbar.image>
# <bitbar.dependencies>zsh</bitbar.dependencies>
SCRIPT_NAME=$(basename $0)
SCRIPT_DIR=$(dirname $0)
typeset -a TZS
TZS=(
"Moscow" "Europe/Moscow"
"London" "Europe/London"
"NewYork" "US/Eastern"
"SanFrancisco" "US/Pacific"
"UTC" "UTC"
)

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
    cur_time=$(get_time "${TZS[TZS[(i)$CUR_CITY]+1]}"); echo "${CUR_CITY} ${cur_time}"
    echo "---"
    i=0
    for city in ${(k)TZS}; do
        ((i++))
        if ((i % 2)); then
            cur_time=$(get_time "${TZS[TZS[(i)$city]+1]}"); echo "$city ${cur_time} | bash="${SCRIPT_DIR}/${SCRIPT_NAME}" param1=chcity param2=$city terminal=false refresh=true"
        fi
    done
    echo "---"
    cur_time=$(date +%s); echo "Epoch $cur_time | bash="${SCRIPT_DIR}/${SCRIPT_NAME}" param1=copy param2=\"$cur_time\" terminal=false"
    cur_time=$(date +'%Y%m%d'); echo "YMD $cur_time | bash="${SCRIPT_DIR}/${SCRIPT_NAME}" param1=copy param2=\"$cur_time\" terminal=false"
    cur_time=$(date -u +"%Y-%m-%dT%H:%M:%SZ"); echo "ISO 8601 $cur_time | bash="${SCRIPT_DIR}/${SCRIPT_NAME}" param1=copy param2=\"$cur_time\" terminal=false"
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