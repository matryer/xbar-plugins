#!/bin/bash
# <bitbar.title>NCMPCPP Name</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>12-Seconds</bitbar.author>
# <bitbar.author.github>12-Seconds</bitbar.author.github>
# <bitbar.desc>Gets name of currently playing song in ncmpcpp</bitbar.desc>
# <bitbar.dependencies>ncmpcpp, mpc</bitbar.dependencies>

sleep 3 && open -g bitbar://refreshPlugin?name=Name.sh
echo "$(/usr/local/bin/mpc current) | terminal=false"
echo "---"
echo "$(/usr/local/bin/mpc)"
