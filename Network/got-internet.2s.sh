#!/bin/bash
# <xbar.title>Got Internet?</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Federico Brigante</xbar.author>
# <xbar.author.github>bfred-it</xbar.author.github>
# <xbar.desc>Checks the connection to Internet and tells you in a single character.</xbar.desc>
# <xbar.image>http://i.imgur.com/I8lF8st.png</xbar.image>

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
