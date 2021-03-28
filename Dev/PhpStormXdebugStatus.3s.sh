#!/bin/bash
#
# <xbar.title>PHPstorm debugger status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Harings Rob</xbar.author>
# <xbar.author.github>haringsrob</xbar.author.github>
# <xbar.desc>Shows the current status of phpstorm debugger</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/haringsrob/</xbar.abouturl>

if lsof -i :9000 | grep -q phpstorm; then
    echo 'Xdebug on | color=#008000'
else
    echo 'Xdebug off | color=#FF0000'
fi
