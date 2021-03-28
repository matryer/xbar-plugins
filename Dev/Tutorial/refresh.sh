#!/bin/bash

# <xbar.title>Refresh Me</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Karl Piper</xbar.author>
# <xbar.author.github>KarlPiper</xbar.author.github>
# <xbar.desc>How to refresh just one plugin, or restart BitBar entirely.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/KarlPiper/Plugins-for-Bitbar/master/images/refresh.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

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