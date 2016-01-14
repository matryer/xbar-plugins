#!/bin/bash

# internal-ip
# BitBar plugin
#
# by Mat Ryer
#
# Gets the current internal IP address, and shows more information in
# the details.

ACTIVE_ADAPTER=`ifconfig | grep \^en | head -n1 | cut -d: -f1`
ipconfig getifaddr $ACTIVE_ADAPTER;
echo "---"
echo "(Internal IP address)"
echo "---"
ifconfig $ACTIVE_ADAPTER;
