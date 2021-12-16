#!/bin/bash

#
# <xbar.title>ARP Spoof Detector</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Tuan Nguyen</xbar.author>
# <xbar.author.github>tuannvm</xbar.author.github>
# <xbar.desc>This plugin detect the ARP spoofing attack on local area network and notify user</xbar.desc>
# <xbar.image>http://i.imgur.com/2jE6g7v.png</xbar.image>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/tree/master/System/arp-spoof-detector.sh</xbar.abouturl>
#

# check if it's there any duplicated arp address
duplicated_arp=$(arp -a | awk '{print $4}' | uniq -d)

if [ -z "$duplicated_arp" ];then
    # if not, print OK, no attack on this network
    echo "OK"
else
    # if there's a duplication, notify the user with the attacker's IP
    result=$(arp -a | grep "$duplicated_arp" | cut -d"(" -f2 | cut -d")" -f1 | tail -n 1)
    echo "ATTACK FROM" "$result" "!"
fi