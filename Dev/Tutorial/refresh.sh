#!/bin/bash

# <bitbar.title>Refresh Me</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Karl Piper</bitbar.author>
# <bitbar.author.github>KarlPiper</bitbar.author.github>
# <bitbar.desc>How to refresh just one plugin, or restart BitBar entirely.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/KarlPiper/Plugins-for-Bitbar/master/images/refresh.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>

if [[ "$1" = "restart" ]]; then
osascript <<EOD
	tell application "BitBar" to quit
	delay 1
	tell application "BitBar" to activate
EOD
fi

echo "↻"
echo "---"
echo "Refresh Me| terminal=false refresh=true"
echo "Restart Bitbar| bash='$0' param1=restart terminal=false";