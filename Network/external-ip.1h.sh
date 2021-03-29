#!/bin/bash

# <xbar.title>external-ip</xbar.title>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Gets the current external IP address.</xbar.desc>

EXTERNAL_IP=$(dig +short myip.opendns.com @resolver1.opendns.com)

if [ "$1" = "copy" ]; then
  # Copy the IP to clipboard
  echo -n "$EXTERNAL_IP" | pbcopy
fi

echo "$EXTERNAL_IP"
echo "---"
echo "(External IP address)"
echo "Copy IP | terminal=false bash='$0' param1=copy"
