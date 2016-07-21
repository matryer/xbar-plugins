#!/usr/bin/env bash
#
# Quickly aria2c
#
# <bitbar.title>aria2c</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>cnsworder</bitbar.author>
# <bitbar.author.github>cnsworder</bitbar.author.github>
# <bitbar.desc>Quickly aria2c</bitbar.desc>

echo "aria2c"
echo "---"
count=$(pgrep "aria2c")
if ((count < 1)); then
    echo 'not running | bash=aria2c param1="-D --enable-rpc --rpc-allow-origin-all --rpc-listen-all -c -x 10 -s 10"'
else
    echo 'runing | color=green  href="http://ziahamza.github.io/webui-aria2/"'
fi
