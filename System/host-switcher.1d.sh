#!/bin/bash
# <bitbar.title>Host file switcher</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>canemacchina</bitbar.author.github>
# <bitbar.author>Lorenzo Bugiani</bitbar.author>
# <bitbar.desc>Simple plugin to switch between multiple hosts files</bitbar.desc>

HOSTS_LOCATION=""

flushcache(){
	killall -HUP mDNSResponder &>/dev/null
	sudo killall mDNSResponderHelper &>/dev/null
	sudo dscacheutil -flushcache &>/dev/null
}

if [ -z "$HOSTS_LOCATION" ]; then
	ABSOLUTE_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
	echo ":warning: Click and readme! | color=red"
	echo "---"
	echo "You have to configure hostSwitcher. | color=red"
	echo "Please open the script and fill correcty HOSTS_LOCATION variable. | color=red"
	echo "Open ${BASH_SOURCE[0]}| color=blue terminal=false bash=/usr/bin/open param1=$ABSOLUTE_PATH"
else
	COMMAND=$1
	HOST_FILE=$2
	if [ ! -z "$COMMAND" ]; then
		case $COMMAND in
			"switch")
				if [ ! -z "$HOST_FILE" ]; then
					/usr/bin/osascript -e "do shell script \"ln -f ${HOST_FILE} /etc/hosts && flushcache\" with administrator privileges"
				fi
				;;
			"open")
				if [ ! -z "$HOST_FILE" ]; then
					open "$HOST_FILE"
				fi
				;;
			"flush")
				/usr/bin/osascript -e "do shell script \"flushcache\" with administrator privileges"
				;;
		esac
	fi

	CURRENT_FILE=$(find "$HOSTS_LOCATION" -samefile '/etc/hosts' -exec basename {} +)

	if [ -z "$CURRENT_FILE" ]; then
		echo ":warning: Click and Readme! | color=red"
		echo "---"
		echo "You have to run hostSwitcher one time. Please select one host file below | color=red"
	else
		echo "$CURRENT_FILE"
	fi
	echo "---"

	cmd=""
	for file in "$HOSTS_LOCATION"/*
	do
		if [ -z "$CURRENT_FILE" ] || [ "$(basename "$file")" != "$CURRENT_FILE" ]; then
			cmd="| terminal=false refresh=true bash=$0 param1=switch param2=$file"
		else
			cmd=""
		fi
		echo "$(basename "$file") $cmd"
		echo "Open $(basename "$file") host file | terminal=false refresh=true alternate=true bash=$0 param1=open param2=$file"
	done

	echo "---"
	echo "Open host files folder | terminal=false refresh=true bash=$0 param1=open param2=$HOSTS_LOCATION"
	echo "Flush dns cache | terminal=false refresh=true bash=$0 param1=flush"

fi
