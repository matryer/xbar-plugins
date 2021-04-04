#!/usr/bin/env bash

# <xbar.title>Bandwidth (KB/s or MB/s)</xbar.title>
# <xbar.version>v0.0.1</xbar.version>
# <xbar.author>Uy Nguyen</xbar.author>
# <xbar.author.github>nguyenvanuyn96</xbar.author.github>
# <xbar.desc>Displays bandwidth usage for the primary interface in MegaBytes/s or KiloBytes/s</xbar.desc>
# <xbar.dependencies>ifstat</xbar.dependencies>
# <xbar.image>https://user-images.githubusercontent.com/13082464/113498791-ba3ef380-9542-11eb-82e4-76e78cac98b7.png</xbar.image>

# based on bandwidth_primary.1s.sh by Kaspars Mickevics

# only gather stats from interface en0
# no need to samlpe unused interfaces
INTERFACE="en0"

if [ ! -e /usr/local/bin/ifstat ]; then
    echo "Please install ifstat or change the path to it in the script."
    exit 1
fi

function kilo_to_mega {
  # in networking 1 mbit is 1000 kilobits (not 1024)
  Kbps=${1}
  KBps=$Kbps/8 # Kilo Bytes 
  if [ "`echo "$KBps < 800.0" | bc`" -eq 1 ]; then
    printf "%0.2f KB/s\n" "$(bc -q <<< scale=3\;"$KBps")"
  elif [ "`echo "$KBps < 1000000.0" | bc`" -eq 1 ]; then
    printf "%0.2f MB/s\n" "$(bc -q <<< scale=3\;"$KBps"/1000)"
  else
    printf "%0.2f GB/s\n" "$(bc -q <<< scale=3\;"$KBps"/1000000)"
  fi;
}

function get_ifstat {
    interface=$1
    # 1 sample for 0.5 second interval
    # outputs two values (in/out) in kilobits per second
    /usr/local/bin/ifstat -n -w -i "${interface}" -b 0.5 1 | tail -n 1
}

function print_ifstat {
    kbits_in=$(echo "$1" | awk '{ print $1 }')
    kbits_out=$(echo "$1" | awk '{ print $2 }')
    mbits_in=$(kilo_to_mega "$kbits_in")
    mbits_out=$(kilo_to_mega "$kbits_out")
    echo "▼ $mbits_in - $mbits_out ▲"

}

print_ifstat "$(get_ifstat ${INTERFACE})"
echo "---"
