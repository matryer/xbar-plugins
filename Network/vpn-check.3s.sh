#!/bin/bash

# <xbar.title>VPN Checker</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Pierre-Louis Dubouilh</xbar.author>
# <xbar.author.github>pldubouilh</xbar.author.github>
# <xbar.desc>Checks the availability of tun0 or ipsec0. Also allows to kill a program on deconnection.</xbar.desc>

# From my infamous one-liner
# ((ifconfig | grep tun0) || (killall Firefox))

if ifconfig | grep -q tun0; then
	echo "VPN ⬆ | color=green"
	ifconfig utun0 2> /dev/null | grep inet | cut -d' ' -f 2
elif ifconfig | grep -q ipsec0; then
	echo "VPN ⬆ | color=green"
	ifconfig ipsec0 2> /dev/null | grep inet | cut -d' ' -f 2
else
	echo "VPN ⬇ | color=red"
	#killall Firefox 2&> /dev/null
fi
