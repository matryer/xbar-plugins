#!/bin/bash

# Display local and external IP and allow copying it. This plugin will connect to icanhazip.com to determine external IP address.
#
# by Martin Braun (martin-braun.net)
#

# metadata
# <xbar.title>IPs</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Martin Braun</xbar.author>
# <xbar.author.github>martin-braun</xbar.author.github>
# <xbar.desc>Display local and external IP and allow copying it. This plugin will connect to icanhazip.com to determine external IP address.</xbar.desc>
# <xbar.image>https://i.imgur.com/8eSN3Hw.png</xbar.image>

locip=`osascript -e "IPv4 address of (system info)"`
pubip=`curl -4s icanhazip.com` > /dev/null

if [[ "$1" = 'copy' ]]; then 
  echo "$2" | tr -d '\n' | pbcopy
fi

echo "${locip/192.168./..}"  
echo "---"
echo "Click on an IP to copy it | font=Tahoma-Bold"
echo "---"
echo "Local:    $locip | font=AndaleMono bash=$0 param1=copy param2=$locip terminal=false refresh=false"
echo "External: $pubip | font=AndaleMono bash=$0 param1=copy param2=$pubip terminal=false refresh=false"
