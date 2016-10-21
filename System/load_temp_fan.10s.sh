#!/bin/bash

#
# <bitbar.title>Load Average/Temperature/Fan Speed</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ivan Kovnatsky</bitbar.author>
# <bitbar.author.github>sevenfourk</bitbar.author.github>
# <bitbar.desc>This plugin displays the current CPU load, CPU temperature, Fan speed (requires iStats ruby gem)</bitbar.desc>
# <bitbar.image>https://i.imgur.com/y3ytf1l.png</bitbar.image>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/load_temp_fan.10s.sh</bitbar.abouturl>
#
# iStats -- https://github.com/Chris911/iStats
# gem install iStats
#

load=$(uptime|sed 's/.*ages: //')
temp=$(/usr/local/bin/istats cpu|awk '{print $3}')
fan=$(/usr/local/bin/istats fan speed|awk '{print $4, $5}')

echo "$load, $temp, $fan"
