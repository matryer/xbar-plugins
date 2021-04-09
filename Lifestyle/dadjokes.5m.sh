#!/bin/bash

# <xbar.title>Dad Jokes</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Simon Peier</xbar.author>
# <xbar.author.github>simonpeier</xbar.author.github>
# <xbar.desc>The plugin tells you random dad jokes</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/simonpeier/bitbar-dadjokes-plugin/master/screenshot.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://simonpeier.github.io/bitbar-dadjokes-plugin/</xbar.abouturl>

joke=$(curl -s -H "Accept: text/plain" https://icanhazdadjoke.com/)

echo "😂"
echo "---"
echo "Next joke | refresh=true"
echo "---"

echo " |trim=false"
echo "$joke"
echo " |trim=false"
