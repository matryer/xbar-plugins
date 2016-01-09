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

export PATH=/usr/local/bin:~/Applications/go/bin:$PATH
json=$(curl -s -L http://rbc.ru | pup '.indicators__group json{}')
out="`echo $json | jq -r '.[] | .children | .[1:] | .[] as $c | "\($c.children | map(.text) | join(" ")) | color=\(if $c.children | .[0] | .text == "▲" then "green" else "red" end) | href=\($c.href)"'`"
echo "$out" | grep Brent
echo "---"
echo "$out" | grep -v Brent | grep -v EUR/USD | grep USD | sed 's/color=green/color=111/;s/color=red/color=green/;s/color=111/color=red/'
echo "$out" | grep -v Brent | grep -v EUR/USD | grep EUR | sed 's/color=green/color=111/;s/color=red/color=green/;s/color=111/color=red/'
echo "$out" | grep EUR/USD
echo "$out" | grep -v Brent | grep -v USD | grep -v EUR
