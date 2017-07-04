#!/bin/bash

#
# <bitbar.title>ARP Spoof Detector</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tuan Nguyen</bitbar.author>
# <bitbar.author.github>tuannvm</bitbar.author.github>
# <bitbar.desc>This plugin detect the ARP spoofing attack on local area network and notify user</bitbar.desc>
# <bitbar.image>http://i.imgur.com/2jE6g7v.png</bitbar.image>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/tree/master/System/arp-spoof-detector.sh</bitbar.abouturl>
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