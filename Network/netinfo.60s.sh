#!/bin/bash
#
# <bitbar.title>Network Info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Raymond Kuiper</bitbar.author>
# <bitbar.author.github>q1x</bitbar.author.github>
# <bitbar.desc>Provides network status information about your Mac: Internal and external IPv4+IPv6 addresses, Whois information and Speedtest.net results.</bitbar.desc>
# <bitbar.dependencies>speedtest-cli</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/zFv3RvI.png</bitbar.image>
#

PATH=/usr/local/bin:$PATH
SPEEDTEST="/tmp/speedtest.txt"

if [ "$1" = "copy" ]; then
  # Copy to clipboard
  echo "$2" | pbcopy
  exit 0
fi

if [ "$1" = "speedtest" ]; then
  # Perform a speedtest
  speedtest-cli --simple --share > "$SPEEDTEST"
  exit 0
fi

#Get external data
EXTERNAL_IP4=$(curl --connect-timeout 3 -s http://v4.ipv6-test.com/api/myip.php || echo None)
EXTERNAL_IP6=$(curl --connect-timeout 3 -s http://v6.ipv6-test.com/api/myip.php || echo None)
INTERFACES=$(ifconfig | grep UP | egrep -o '(^en[0-9]*|^utun[0-9]*)' | sort -n)
[[ "$EXTERNAL_IP4" == "None" ]] && WHOIS="" || WHOIS=$(whois $EXTERNAL_IP4 | awk '/descr: / {$1=""; print $0 }' | head -n 1)


# Start output
[[ "$EXTERNAL_IP4" == "None" ]] && echo "❌" || echo "🌐"
echo "---"
echo "Public: "
echo "IPv4: ${EXTERNAL_IP4}${WHOIS} | terminal=false bash=$0 param1=copy param2=$EXTERNAL_IP4"
echo "IPv6: ${EXTERNAL_IP6} | terminal=false bash=$0 param1=copy param2=$EXTERNAL_IP6"
echo "---"
echo "📈 Perform Speedtest | color=blue terminal=false refresh=true bash=$0 param1=speedtest"
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

echo "---"
for INT in $INTERFACES; do
     echo "$INT:"
     ifconfig $INT | awk '/ether/ { print "MAC: " $2 "| terminal=false bash=$0 param1=copy param2=" $2 }; /inet / { print "IPv4: " $2 "| terminal=false bash=$0 param1=copy param2=" $2 };  /inet6/ { print "IPv6: " $2 "| terminal=false bash=$0 param1=copy param2=" $2 }' | sed 's/%.*\|/\|/'
     #[[ "$IP" != "" ]] && echo "$INT: $IP | terminal=false bash=$0 param1=copy param2=$IP"
     echo "---"
done
