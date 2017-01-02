#!/bin/bash

# <bitbar.title>external-ip</bitbar.title>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Gets the current external IP address.</bitbar.desc>

EXTERNAL_IP=$(dig +short myip.opendns.com @resolver1.opendns.com)

if [ "$1" = "copy" ]; then
  # Copy the IP to clipboard
  echo -n "$EXTERNAL_IP" | pbcopy
fi

echo "$EXTERNAL_IP"
echo "---"
echo "(External IP address)"
echo "Copy IP | terminal=false bash='$0' param1=copy"
