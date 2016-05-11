#!/bin/bash

# <bitbar.title>internal-ip</bitbar.title>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Gets the current internal IP address, and shows more information in the details.</bitbar.desc>
# <bitbar.version>1.0</bitbar.version>

ACTIVE_ADAPTER=$(ifconfig | grep ^en | head -n1 | cut -d: -f1)
ipconfig getifaddr "$ACTIVE_ADAPTER";
echo "---"
echo "(Internal IP address)"
echo "---"
ifconfig "$ACTIVE_ADAPTER";
