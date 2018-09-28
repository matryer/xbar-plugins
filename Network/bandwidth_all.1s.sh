#!/usr/bin/env bash

# <bitbar.title>Bandwidth (Mbit/s)</bitbar.title>
# <bitbar.version>v0.0.2</bitbar.version>
# <bitbar.author>Dan Rohtbart</bitbar.author>
# <bitbar.author.github>danrohtbart</bitbar.author.github>
# <bitbar.desc>Displays total bandwidth usage for all interfaces in Megabits/s. Especially useful if you are frequently switching between wi-fi and ethernet connections.</bitbar.desc>
# <bitbar.dependencies>ifstat</bitbar.dependencies>

# based on bandwidth.1s.sh by Ant Cosentino
# based on bandwidth_primary.1s.sh by Kaspars Mickevics


if [ ! -e /usr/local/bin/ifstat ]; then
    echo "Please install ifstat or change the path to it in the script."
    exit 1
fi

function kilo_to_mega {
  # in networking 1 mbit is 1000 kilobits (not 1024)
  printf "%0.1f\\n" "$(bc -q <<< scale=3\;"${1}"/1000)"
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
    echo "▼$mbits_in-$mbits_out▲"
}

INTERFACES=$(ifconfig -lu)

TOTAL_KBITS_IN=0
TOTAL_KBITS_OUT=0

for INTERFACE in ${INTERFACES}
  do
    IFSTAT_OUTPUT="$(get_ifstat "${INTERFACE}")"
    KBITS_IN=$(echo "$IFSTAT_OUTPUT" | awk '{ print $1 }')
    KBITS_OUT=$(echo "$IFSTAT_OUTPUT" | awk '{ print $2 }')
    if [ "$KBITS_IN" != "n/a" ]
    then
      TOTAL_KBITS_IN=$(echo "$TOTAL_KBITS_IN + $KBITS_IN" | bc)
      TOTAL_KBITS_OUT=$(echo "$TOTAL_KBITS_OUT + $KBITS_OUT" | bc)
    fi
	done

print_ifstat "$TOTAL_KBITS_IN $TOTAL_KBITS_OUT"
