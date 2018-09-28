#!/bin/bash
#
# <bitbar.title>PHPstorm debugger status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Harings Rob</bitbar.author>
# <bitbar.author.github>haringsrob</bitbar.author.github>
# <bitbar.desc>Shows the current status of phpstorm debugger</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/haringsrob/</bitbar.abouturl>

if [[ $(lsof -i :9000) ]]; then
    echo 'Xdebug on | color=#008000'
else
    echo 'Xdebug off | color=#FF0000'
fi
