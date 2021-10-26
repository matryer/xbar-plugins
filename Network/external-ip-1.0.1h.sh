#!/bin/bash

# <xbar.title>My External IP</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>CodinCafe</xbar.author>
# <xbar.author.github>codincafe</xbar.author.github>
# <xbar.desc>This plugin will show your current external / public IP and allow you to copy the same to clipboard</xbar.desc>
# <xbar.image>https://i.imgur.com/Ar1JABA.png</xbar.image>
# <xbar.dependencies>curl</xbar.dependencies>

echo "🌎"
echo '---' 

if [ $(curl -LI http://google.com -o /dev/null -w '%{http_code}\n' -s) == "200" ]; then
    OUTPUT=$(curl -s https://ipinfo.io/ip)
    result_string="$OUTPUT | bash='/bin/bash' param1='-c' param2='/bin/echo $OUTPUT | pbcopy' terminal=false";
else
    result_string="No Internet Connection"
fi

echo "$result_string"
echo ---
echo "Refresh... | refresh=true"
