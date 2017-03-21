#!/bin/bash
# <bitbar.title>octoprint</bitbar.title>
# <bitbar.author>makoto abe</bitbar.author>
# <bitbar.author.github>m0a</bitbar.author.github>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.desc>3dprinter control with octoprint api</bitbar.desc>
# <bitbar.image>http://i.imgur.com/daHLotM.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# please setup apikey
APIKEY="222626A0B0794703A203D5E976BE0AEC"
ENDPOINT="http://192.168.11.111:5000"
JQ=/usr/local/bin/jq


if [ ! -e $JQ ]; then 
    echo "expect 'brew install jq'" 
    exit 1
fi
DEBUG=false
HEADER="x-api-key:$APIKEY"


function runapi {
  curl -s -gH "$HEADER" "$ENDPOINT/api/$1"
  return 0
}

function displaytime {
  local T=$1
  local D=$((T/60/60/24))
  local H=$((T/60/60%24))
  local M=$((T/60%60))
  local S=$((T%60))
  
  (( D > 0 )) && printf '%dd' $D
  (( H > 0 )) && printf '%dh' $H
  (( M > 0 )) && printf '%dm' $M
  (( D > 0 || H > 0 || M > 0 )) && printf '%ds | color=black\n' $S
  return 0
}


# menu functions
function printcmd {
    curl -s -H $HEADER -H Accept:application/json -H Content-type:application/json -X POST -d '{"command":"select","print":true}' "$ENDPOINT/api/files/local/$1"
}
function printstopcmd {
    curl -s -H $HEADER -H Accept:application/json -H Content-type:application/json -X POST -d '{"command":"cancel"}' $ENDPOINT/api/job
}
function deletecmd {
    curl -s -H $HEADER  -X DELETE  "$ENDPOINT/api/files/local/$1"
}

# task switch if parameter count = 0 then print menu
if [ $# -ne 0 ]; then
    cmd=$1
    echo "run $cmd $2"
    status=$($cmd "$2")
    echo "$status"
    exit 0
fi


# env check
job=$(runapi job)
if [ "$job" = '' ]; then
    echo "please edit this file and change ENDPOINT."
    exit 1;
fi

if [ "$job" = 'Invalid API key' ]; then
    echo "please edit this file and change APIKEY."
    exit 1;
fi

seconds=$(echo "$job" | $JQ .progress.printTimeLeft)
filename=$(echo "$job" |$JQ .job.file.name -r)
state=$(echo "$job" |$JQ .state -r)

if [ "$state" = Printing ]; then
printf 'ETE ' && displaytime "$seconds" 

echo "$filename | color=black"
else 
echo "$state | color=black"
fi


echo "---"

printer=$(runapi printer)
temp0=$(echo "$printer" |$JQ .temperature.tool0.actual -r)
bed=$(echo "$printer" |$JQ .temperature.bed.actual -r)
echo "hotend:$temp0°C  bed:$bed°C | color=black"


if [ "$DEBUG" = true ]; then echo "Refresh | refresh=true" ; fi

version=$(runapi version)
octo_ver=$(echo "$version" |$JQ -r .server)
api_ver=$(echo "$version" |$JQ -r .api)
echo "octoprint:$octo_ver api:$api_ver | color=green href=$ENDPOINT"

# control job
if [ "$state" = Printing ]; then
echo "---"
echo "print cancel  | color=red bash=$0 param1=printstopcmd  terminal=$DEBUG"
fi

echo "---"
files=$(runapi files)
filenames=$(echo "$files" |$JQ -r '.files | sort_by(.date)| reverse | .[].name')

# echo "$files" | pbcopy


function filesubmenu {
    local filename=$1
    local onefile
    local filesize
    local filedate
    
    # shellcheck disable=SC2016
    onefile=$(echo "$files" |$JQ  --arg filename "$filename" '.files | map(select(.name == $filename ))' )
    
    filesize=$(echo "$onefile" |$JQ .[0].size)
    filedate=$(echo "$onefile" |$JQ .[0].date)

    # print submenu
    echo "$filename"
    
    if [ "$state" != "Printing" ]; then
        echo "--start print | color=green bash=$0 param1=printcmd param2=$filename refresh=true terminal=$DEBUG"
    fi
    echo "--date: $filedate"
    echo "--size: $filesize"
    echo "--delete | color=green bash=$0 param1=deletecmd param2=$filename refresh=true terminal=$DEBUG "
    return 0
}

# show file submenu
for f in $filenames;do
    filesubmenu "$f"
done
