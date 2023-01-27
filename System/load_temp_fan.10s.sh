#!/bin/bash

#
# <xbar.title>Load Average/Temperature/Fan Speed</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ivan Kovnatsky</xbar.author>
# <xbar.author.github>sevenfourk</xbar.author.github>
# <xbar.desc>This plugin displays the current CPU load, CPU temperature, Fan speed (requires iStats ruby gem)</xbar.desc>
# <xbar.image>https://i.imgur.com/y3ytf1l.png</xbar.image>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/System/load_temp_fan.10s.sh</xbar.abouturl>
#
# iStats -- https://github.com/Chris911/iStats
# gem install iStats
#

load=$(uptime|sed 's/.*ages: //')
temp=$(/usr/local/bin/istats cpu|awk '{print $3}')
fan=$(/usr/local/bin/istats fan speed|awk '{print $4, $5}')

echo "$load, $temp, $fan"
