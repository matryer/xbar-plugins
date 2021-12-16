#!/usr/bin/env bash

# <xbar.title>Bandwidth (Mbit/s)</xbar.title>
# <xbar.version>v0.0.1</xbar.version>
# <xbar.author>Kaspars Mickevics</xbar.author>
# <xbar.author.github>fxlv</xbar.author.github>
# <xbar.desc>Displays bandwidth usage for the primary interface in Megabits/s</xbar.desc>
# <xbar.dependencies>ifstat</xbar.dependencies>
# <xbar.image>https://cloud.githubusercontent.com/assets/2462211/12748504/584bbcea-c9b3-11e5-8109-ad8fdcefdc75.png</xbar.image>

# based on bandwidth.1s.sh by Ant Cosentino

# only gather stats from interface en0
# no need to samlpe unused interfaces
INTERFACE="en0"


if [ ! -e /usr/local/bin/ifstat ]; then
    echo "Please install ifstat or change the path to it in the script."
    exit 1
fi

function kilo_to_mega {
  # in networking 1 mbit is 1000 kilobits (not 1024)
  printf "%0.3f\n" "$(bc -q <<< scale=3\;"${1}"/1000)"
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
