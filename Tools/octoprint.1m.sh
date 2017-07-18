#!/bin/bash
# <bitbar.title>Octoprint</bitbar.title>
# <bitbar.author>m0a</bitbar.author>
# <bitbar.author.github>m0a</bitbar.author.github>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.desc>3dprinter control with octoprint api</bitbar.desc>
# <bitbar.image>http://i.imgur.com/biN2ISe.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# please setup 
APIKEY="222626A0B0794703A203D5E976BE0AEC"
ENDPOINT="http://192.168.11.111:5000"
SNAPSHOTURL="http://192.168.11.111:8080/?action=snapshot"
IMAGEWIDTH=300x

JQ=/usr/local/bin/jq
CONVERT=/usr/local/bin/convert


if [ ! -e $JQ ]; then 
		echo "expect 'brew install jq'" 
		exit 1
fi
if [ ! -e $CONVERT ]; then 
		echo "expect 'brew install imagemagick'" 
		exit 1
fi


DEBUG=false
HEADER="x-api-key:$APIKEY"


function runapi {
	curl -s -gH "$HEADER" "$ENDPOINT/api/$1"
	return 0
}

function displaybytes {
    local bytes=$1;
    if [[ $bytes -lt 1048576 ]]; then
        echo "$(( (bytes + 1023)/1024 ))KiB"
    else
        echo "$(( (bytes + 1048575)/1048576 ))MiB"
    fi
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

function displaysnapshot {
		local snapshot
		if ! curl -s "$SNAPSHOTURL" > /dev/null
		then
			echo "webcam don't enable"
			return 1
		fi
		snapshot=$(curl -s "$SNAPSHOTURL" | $CONVERT - -quiet -resize $IMAGEWIDTH - |base64)
		echo "| image=$snapshot refresh=true"
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
		echo "err | color=red"
		echo "---"
		echo "please edit this file and change ENDPOINT."
		exit 1;
fi

if [ "$job" = 'Invalid API key' ]; then
		echo "err | color=red"
		echo "---"
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

displaysnapshot

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
		filesize=$(displaybytes "$filesize")
		
		filedate=$(echo "$onefile" |$JQ .[0].date)
		filedate=$(date -r "$filedate" +"%Y-%m-%d %H:%M:%S" )

		# print submenu
		echo "$filename"
		
		if [ "$state" != "Printing" ]; then
				echo "--start print | color=green bash=$0 param1=printcmd param2=$filename refresh=true terminal=$DEBUG"
		fi
		echo "--uploaded: $filedate"
		echo "--size: $filesize"
		echo "--delete | color=green bash=$0 param1=deletecmd param2=$filename refresh=true terminal=$DEBUG "
		return 0
}

# show file submenu
for f in $filenames;do
		filesubmenu "$f"
done
