#!/bin/bash
# File: rbc.15.m.sh
# Description: Plugin for BitBar (github.com/matryer/bitbar) displaying russian
#              stock-market indicators from rbc.ru
# Author: github.com/schmooser
# Dependencies:
#   jq - github.com/stedolan/jq
#   pup - github.com/EricChiang/pup
#   curl - curl.haxx.se
# They should be added in PATH
# <xbar.title>RBC Stock/Currency tracker</xbar.title>
# <xbar.author>schmooser</xbar.author>
# <xbar.author.github>schmooser</xbar.author.github>
# <xbar.image>https://cloud.githubusercontent.com/assets/480160/12206066/ee9db3be-b64f-11e5-96ae-11f7678cb905.png</xbar.image>
# <xbar.desc>russian stock-market indicators from rbc.ru</xbar.desc>
# <xbar.dependencies>jq,pup,curl</xbar.dependencies>
# <xbar.version>1.0</xbar.version>

export PATH=/usr/local/bin:~/Applications/go/bin:$PATH
json=$(curl -s -L http://rbc.ru | pup '.indicators__items json{}')
# shellcheck disable=2016
out=$(echo "$json" | jq -r '.children | .[] as $c | "\(if $c.children | .[2] | .class | contains("green") then "▲" else "▼" end) \($c.title) - \($c.children[1:] | map(.text) | join(" ")) | color=\(if $c.children | .[2] | .class | contains("green") then "green" else "red" end) | href=\($c.href)"')
echo "$out" | grep Brent
echo "---"
echo "$out" | grep -v Brent | grep -v EUR/USD | grep USD | sed 's/color=green/color=111/;s/color=red/color=green/;s/color=111/color=red/'
echo "$out" | grep -v Brent | grep -v EUR/USD | grep EUR | sed 's/color=green/color=111/;s/color=red/color=green/;s/color=111/color=red/'
echo "$out" | grep EUR/USD
echo "$out" | grep -v Brent | grep -v USD | grep -v EUR
