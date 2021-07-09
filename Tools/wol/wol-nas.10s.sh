#!/bin/bash

#  <xbar.title>Wake on LAN (WoL)</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Michael R.</xbar.author>
#  <xbar.author.github>The-Michael-R</xbar.author.github>
#  <xbar.desc>Small xbar plugin to wake one server/computer using a WoL packet. Needs https://github.com/jpoliv/wakeonlan (brew or place the file wakeonlan to /urs/local/bin</xbar.desc>
#  <xbar.image>https://github.com/The-Michael-R/xbar-plugins/Tools/wol/wol_xbar.png</xbar.image>
#  <xbar.dependencies>bash, wakeonlan (brew install)</xbar.dependencies>

# Parameters (please use use the MAC/IP-Address of your server)
# <xbar.var>string(VAR_IP="127.0.0.1"): Server-IP.</xbar.var>
# <xbar.var>string(VAR_MAC="12:34:56:78:AB:CD"): Server-MAC address.</xbar.var>

GREEN="\033[32m"
RED="\033[31m"
NORMAL="\033[0m"

if ping -t 1 -c 1 ${VAR_IP} &> /dev/null
then
    echo -e "WOL${GREEN}⇡${NORMAL}"
    echo "---"
    echo "NAS is UP"
else
    echo -e "WOL${RED}⇣${NORMAL}"
    echo "---"
    echo "Wake NAS | shell='/usr/local/bin/wakeonlan' param1='${VAR_MAC}' "
fi

