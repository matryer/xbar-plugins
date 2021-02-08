#!/bin/bash

# <bitbar.title>internal-ip</bitbar.title>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Gets the current internal IP address, and shows more information in the details.</bitbar.desc>
# <bitbar.version>1.0</bitbar.version>

ifconfig | grep -e "^en" | sort -u | while read line ; do
    ACTIVE_ADAPTER=$(echo $line | awk -F: '{print $1}')
    INTERNAL_IP_ADDRESS=$(ifconfig $ACTIVE_ADAPTER | grep "inet " | awk '{print $2}')
    if [ -n "$INTERNAL_IP_ADDRESS" ]; then
        echo $INTERNAL_IP_ADDRESS
        echo "---"
        echo "(Internal IP address)"
        echo "---"
        ifconfig ${ACTIVE_ADAPTER}

        break
    fi
done
