#!/bin/bash
# <bitbar.title>TRIPPing</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Anastasios Monachos (secuid0) - [anastasiosm(at)gmail(dot)com]</bitbar.author>
# <bitbar.author.github>secuid0</bitbar.author.github>
# <bitbar.desc>TRIPPing (TRaceroute-IP-Ping) plugin aims to regularly monitor the path and ping roundtrip, from your network to fixed IPs and determine if they have suddenly been changed. Greetz to Kim DotCom</bitbar.desc>
# <bitbar.image>https://i.imgur.com/eKxeuxq.png</bitbar.image>
# <bitbar.dependencies>bash, osascript</bitbar.dependencies>

# default refresh interval 1h, to change, rename this file accordingly:
# traceroute-hops-counter.{time}.sh
# where {time} can be a integer following by:
# s for second(s)
# m for minute(s)
# h for hour(s)
# d for day(s)

# Notify the user
notify () {
    osascript -e "display notification \"Number of hops: $1\" with title \"TRIPPing\""	
}

# Get our public IPv4 and IPv6 addresses
EXTERNAL_IP4=$(curl -4 --connect-timeout 3 -s http://v4.ipv6-test.com/api/myip.php || echo None)
EXTERNAL_IP6=$(curl -6 --connect-timeout 3 -s http://v6.ipv6-test.com/api/myip.php || echo None)

# GUI Bar
[[ "$EXTERNAL_IP4" == "None" && "$EXTERNAL_IP6" == "None" ]]  && echo ":negative_squared_cross_mark:" || echo ":space_invader:	"

# Sites to ping
SITES_TO_PING=(dns.google www.wikipedia.org www.pastebin.com)

# Traceroute 1.1.1.1
TRACEROUTE=$(traceroute 1.1.1.1 2>/dev/null)

# GUI
echo "---"
echo ":arrows_counterclockwise: Refresh... | color=black refresh=true"
echo "---"
echo "Public IPs: | color=red"
echo "IPv4: ${EXTERNAL_IP4} | color=black terminal=false bash='$0' param2=$EXTERNAL_IP4"
echo "IPv6: ${EXTERNAL_IP6} | color=black terminal=false bash='$0' param2=$EXTERNAL_IP6"
echo "---"
echo "Ping output: | color=red"
for i in "${SITES_TO_PING[@]}"
do
	ping -c 1 "$i" |grep -vE 'transmitted|statistics|ttl' | sed 's/---//g' | sed '/^$/d'
   	echo "-"
done
echo "---"
echo "Traceroute output: | color=red"
echo "${TRACEROUTE}"

HOPS=$(echo "${TRACEROUTE}" | awk -F " " 'END {print $1}')
notify "$HOPS"
