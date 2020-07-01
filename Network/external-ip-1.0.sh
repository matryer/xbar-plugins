#!/bin/bash

# <bitbar.title>My External IP</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>CodinCafe</bitbar.author>
# <bitbar.author.github>codincafe</bitbar.author.github>
# <bitbar.desc>This plugin will show your current external / public IP and allow you to copy the same to clipboard</bitbar.desc>
# <bitbar.image>https://i.imgur.com/Ar1JABA.png</bitbar.image>
# <bitbar.dependencies>curl</bitbar.dependencies>

echo "🌎"
echo '---'
OUTPUT=$(curl -s https://ipinfo.io/ip)

result_string="$OUTPUT | bash='/bin/bash' param1='-c' param2='/bin/echo $OUTPUT | pbcopy' terminal=false"
echo "$result_string"
