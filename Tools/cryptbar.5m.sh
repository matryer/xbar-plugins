#!/bin/bash
# <bitbar.title>CryptBar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Simon Egerland</bitbar.author>
# <bitbar.author.github>warmup72</bitbar.author.github>
# <bitbar.desc>Automounter for GoCryptFS</bitbar.desc>
# <bitbar.image>http://warmup.mypump.de/cryptbar10.jpg</bitbar.image>
# <bitbar.dependencies>Shell-Script for GoCryptFS</bitbar.dependencies>

# letzten BAckslash im Pfad wenn vorhanden entfernen
# Mount - Fehler anzeigen, wenn Busy

gocryptfspath="/usr/local/bin/"
homepath=$(defaults read com.matryer.Bitbar | grep "pluginsDirectory" | cut -d"\"" -f 2)

# --- Functions ---
function checkmount {
	color[$i]="red"
	status[$i]="not mounted | color=HotPink "
	if [ ! -d "${crypt[$i]}" ]; then pathcheck[$i]="Crypt-Path not found\n"; fi
	if [ ! -d "${mountpoint[$i]}" ]; then pathcheck[$i]="${pathcheck[$i]}Mount-Path not found\n"; fi
	if [ ! -e "${password[$i]}" ]; then pathcheck[$i]="${pathcheck[$i]}Password-File not found"; fi
	if [ "${pathcheck[$i]}" == "" ]; then
    	check=$(df -h | grep -i -c -a "${mountpoint[$i]}")
    	if [ "$check" != "1" ]; then
			color[$i]="gainsboro"
    	else
			color[$i]="black"
			status[$i]="mounted | color=DarkGreen "
    	fi
	fi
}

function read_para {
	if [ ! -e "${homepath}/.cryptbar_para" ]; then
		echo "#Path to Crypt-Folder;Mounting Point;Password-File;Auto Mount [on|off];Parameters for gocryptfs (optional);" > "$homepath/.cryptbar_para"
		echo "CryptPath;Mountpoint;Password-File;AutoMount;Params;" >> "$homepath/.cryptbar_para"
	fi
	i=0
	while read -r line; do
		if [ "${line:0:1}" == "#" ]; then continue; fi
		crypt[$i]=$(cut -d';' -f 1 <<< "${line}"); crypt[$i]=${crypt[$i]%/}
		#if [ "${crypt[$i]:(-1)}" == "/" ]; then crypt[$i]=$(echo "${crypt[$i]%%?}"); fi
    	mountpoint[$i]=$(cut -d';' -f 2 <<< "${line}"); mountpoint[$i]=${mountpoint[$i]%/}
		#if [ "${mountpoint[$i]:(-1)}" == "/" ]; then mountpoint[$i]=$(echo "${mountpoint[$i]%%?}"); fi
    	password[$i]=$(cut -d';' -f 3 <<< "${line}"); password[$i]=${password[$i]%/}
		auto[$i]=$(cut -d';' -f 4 <<< "${line}")
    	params[$i]=$(cut -d';' -f 5 <<< "${line}")
    	if [ "${params[$i]}" == "" ]; then params[$i]="-q"; fi
		checkmount
		i=$((i + 1))
	done < "$homepath/.cryptbar_para"
	last=$((i - 1))
}

function write_para {
	echo "#Path to Crypt-Folder;Mounting Point;Password-File;Auto Mount [on|off];Parameters for gocryptfs (optional);" > "$homepath/.cryptbar_para"
	#for((i=0; i<${#crypt[*]}; i++))
	for((i=0; i<=last; i++))
	do
		echo "${crypt[$i]};${mountpoint[$i]};${password[$i]};${auto[$i]};${params[$i]};" >> "$homepath/.cryptbar_para"
	done
}

function mount {
	if [ "${status[$i]:0:7}" != "mounted" ]; then
		line="-passfile=${password[$i]} ${params[$i]} ${crypt[$i]} ${mountpoint[$i]}"
		$gocryptfspath/gocryptfs -passfile="${password[$i]}" "${params[$i]}" "${crypt[$i]}" "${mountpoint[$i]}" &>/dev/null
		code="$?"
		checkmount
		if	[ $code -ne 0 ]; then
			prefix="⚠️"
			error[$i]="⚠️"
			osascript -e 'display notification "'"Error $code mounting ${mountpoint[$i]} "'" with title "'"GoCryptFS-Mounter"'" sound name "glass"'
			color[$i]="red"
		else
			prefix=""
			error[$i]=""
		fi
	fi
}

function menu {
	echo "${prefix}🔐"
	echo "---"
	echo "Mount Point Path | color=black"
	for((i=0; i<${#menu[*]}; i++))
	do
		echo "${menu[$i]}"
		echo "${button[$i]}"
		if [ "${button2[$i]}" != "" ]; then echo "${button2[$i]}"; fi
		echo "--Info | color=black"
		echo "----Crypt Path: ${crypt[$i]} | color=RoyalBlue"
		echo "----Mounted on: ${mountpoint[$i]} | color=RoyalBlue"
		echo "----Auto Mount: ${auto[$i]} | color=black bash='$0' param1=autoswitch param2='$i' terminal=false refresh=true>"
		echo "----Status: ${status[$i]}"
	done
	echo "---"
	echo "Edit Table | color=black bash='$0' param1=edit terminal=false refresh=true>"
}

# --- Buttons ---
if [ "$1" = 'mount' ]; then
    read_para
	i=$2
	auto[$i]="on"
	mount
    exit
fi

if [ "$1" = 'umount' ]; then
    read_para
	i=$2
	umount "${crypt[$i]}" &>/dev/null
	code="$?"
	if	[ $code -ne 0 ]; then
		osascript -e 'display notification "'"Error $code - Drive is busy!"'" with title "'"GoCryptFS-Mounter"'" sound name "glass"'
    fi
	exit
fi

if [ "$1" = 'open' ]; then
    read_para
	i=$2
	open "${mountpoint[$i]}"
    exit
fi

if [ "$1" = 'edit' ]; then
    open "${homepath}/.cryptbar_para"
    exit
fi

if [ "$1" = 'autoswitch' ]; then
    read_para
	i=$2
	if [ "${auto[$i]}" = "on" ]; then auto[$i]="off"; else auto[$i]="on"; fi
	write_para
    exit
fi

# --- MAIN ---
read_para
for((i=0; i<=last; i++))
do
	if [ "${auto[$i]}" == "on" ] && [ "${color[$i]}" != "red" ]; then
    	mount
	fi
	if	[ "${pathcheck[$i]}" != "" ]; then
		button[$i]=""
		button2[$i]="--${pathcheck[$i]}"
	else
		if	[ "${status[$i]:0:7}" != "mounted" ]; then
			button[$i]=""
			button2[$i]="--Mount | color=black bash='$0' param1=mount param2='$i' terminal=false refresh=true>"
		else
			button[$i]="--Open | color=black bash='$0' param1=open param2='$i' terminal=false refresh=true>"
			if [ "${auto[$i]}" != "on" ]; then
				button2[$i]="--Umount | color=black bash='$0' param1=umount param2='$i' terminal=false refresh=true>"
			else
				button2[$i]=""
			fi
		fi
	fi
	menu[$i]="   ▶︎ ${error[$i]}${mountpoint[$i]} | color=${color[$i]} trim=false"
done

menu

exit
#----------------------------------------------
