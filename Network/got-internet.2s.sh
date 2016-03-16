#!/bin/bash
# <bitbar.title>Got Internet?</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Federico Brigante</bitbar.author>
# <bitbar.author.github>bfred-it</bitbar.author.github>
# <bitbar.desc>Checks the connection to Internet and tells you in a single character.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/I8lF8st.png</bitbar.image>

ping_timeout=1 #integers only, ping's fault
ping_address=8.8.8.8

if ! ping -c 1 -t $ping_timeout -q $ping_address > /dev/null 2>&1; then
	echo "✧|color=#f23400 dropdown=false"
	echo "---"
	echo "You're offline"
	# echo "Ping to Google DNS failed"
else
	echo "✦|dropdown=false"
	echo "---"
	echo "You're online"
fi
