#!/usr/bin/env bash
#
# Quickly aria2c
#
# <xbar.title>aria2c</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>cnsworder</xbar.author>
# <xbar.author.github>cnsworder</xbar.author.github>
# <xbar.desc>Quickly aria2c</xbar.desc>

echo "aria2c"
echo "---"
count=$(pgrep "aria2c")
if ((count < 1)); then
    echo 'not running | bash=aria2c param1="-D --enable-rpc --rpc-allow-origin-all --rpc-listen-all -c -x 10 -s 10"'
else
    echo 'runing | color=green  href="http://ziahamza.github.io/webui-aria2/"'
fi
