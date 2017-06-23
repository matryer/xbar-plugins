#!/bin/bash
#
# <bitbar.title>Network Info</bitbar.title>
# <bitbar.version>v1.01</bitbar.version>
# <bitbar.author>Raymond Kuiper</bitbar.author>
# <bitbar.author.github>q1x</bitbar.author.github>
# <bitbar.desc>Provides network status information about your Mac: Internal and external IPv4+IPv6 addresses, Whois information and Speedtest.net results.</bitbar.desc>
# <bitbar.dependencies>speedtest-cli</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/zFv3RvI.png</bitbar.image>
#
#
# This bitbar plugin was based on the original "external-ip" Bitbar plugin by Mat Ryer.
# A lot of new functionality has been added, including adding support for speedtest.net and listing internal interface information.
#

# Set path and Speedtest tmp file
PATH=/usr/local/bin:$PATH
SPEEDTEST="/tmp/speedtest.txt"


# Function to notify the user via Aple Script
notify () {
    osascript -e "display notification \"$1\" with title \"Netinfo\""
}

# If called with parameter "copy", copy the second parameter to the clipboard
if [ "$1" = "copy" ]; then
  # Copy to clipboard
  echo "$2" | pbcopy
  notify "Copied $2 to clipboard"
  exit 0
fi

# If called with parameter "speedtest", run speedtest-cli
if [ "$1" = "speedtest" ]; then
  # test if speedtest-cli is found
  if [[ "$(which speedtest-cli)" != "" ]]; then
    # Perform a speedtest
    if speedtest-cli --simple --share > "$SPEEDTEST"; then
      notify "Speedtest is finished"
    else
      notify "Speedtest failed"
    fi
  else
     notify "Speedtest-cli not found!"
  fi
  exit 0
fi

# Get external IPs
EXTERNAL_IP4=$(curl -4 --connect-timeout 3 -s http://v4.ipv6-test.com/api/myip.php || echo None)
EXTERNAL_IP6=$(curl -6 --connect-timeout 3 -s http://v6.ipv6-test.com/api/myip.php || echo None)

# Perform whois lookup on the external IPv4 address.
[[ "$EXTERNAL_IP4" == "None" ]] && WHOIS="" || WHOIS=$(whois "$EXTERNAL_IP4" | awk '/descr: / {$1=""; print $0 }' | head -n 1)

# Find interfaces
INTERFACES=$(ifconfig | grep UP | egrep -o '(^en[0-9]*|^utun[0-9]*)' | sort -n)

# Start building output
[[ "$EXTERNAL_IP4" == "None" && "$EXTERNAL_IP6" == "None" ]]  && echo "❌" || echo "🌐"
echo "---"
echo "🔄 Refresh | colo=black refresh=true"
echo "---"
echo "Public: "
echo "IPv4: ${EXTERNAL_IP4}${WHOIS} | terminal=false bash='$0' param1=copy param2=$EXTERNAL_IP4"
echo "IPv6: ${EXTERNAL_IP6} | terminal=false bash='$0' param1=copy param2=$EXTERNAL_IP6"
echo "---"
echo "📈 Perform Speedtest | terminal=false refresh=true bash='$0' param1=speedtest"

# Pretty format the last speedtest if the tmp file is found
if [[ -e "$SPEEDTEST" ]]; then
     LAST=$(stat -f "%Sm" -t "%Y-%m-%d %H:%M:%S" "$SPEEDTEST")
     PING=$(awk '/Ping: / { $1=""; print $0 }' "$SPEEDTEST")
     UP=$(awk '/Upload: / { $1=""; print $0 }' "$SPEEDTEST")
     DOWN=$(awk '/Download: / { $1=""; print $0 }' "$SPEEDTEST")
     LINK=$(awk '/Share results: / { $1=""; $2=""; print $0 }' "$SPEEDTEST")
     echo "Last checked: $LAST"
     [[ "$PING" != "" ]] && echo "⏱$PING ▼$DOWN ▲$UP | href=$LINK"|| echo "No results..."
else
     echo "Last checked: Never"
fi

# Loop through the interfaces and output MAC, IPv4 and IPv6 information
echo "---"
for INT in $INTERFACES; do
     echo "$INT:"
     ifconfig "$INT" | awk "/ether/ { print \"MAC: \" \$2 \" | terminal=false bash='$0' param1=copy param2=\" \$2 }; /inet / { print \"IPv4: \" \$2 \" | terminal=false bash='$0' param1=copy param2=\" \$2 };  /inet6/ { print \"IPv6: \" \$2 \" | terminal=false bash='$0' param1=copy param2=\" \$2 }" | sed -e 's/%utun[0-9]*//g' -e 's/%en[0-9]*//g' | sort
     echo "---"
done

# EoF
