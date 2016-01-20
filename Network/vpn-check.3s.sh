#!/bin/bash

# <bitbar.title>VPN Checker</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Pierre-Louis Dubouilh</bitbar.author>
# <bitbar.author.github>pldubouilh</bitbar.author.github>
# <bitbar.desc>Checks the availability of tun0. Also allows to kill a program on deconnection.</bitbar.desc>

# From my infamous one-liner
# ((ifconfig | grep tun0) || (killall Firefox))

if [[ `ifconfig | grep tun0` ]]; then
	echo "VPN ⬆ | color=green"
	ifconfig utun0 2> /dev/null | grep inet | cut -d' ' -f 2 
else
	echo "VPN ⬇ | color=red"
	#killall Firefox 2&> /dev/null
fi
