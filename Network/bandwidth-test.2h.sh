#!/bin/sh
#
# Bandwith test, using speedtest-cli (https://github.com/sivel/speedtest-cli) 
#
# <bitbar.title>Bandwith test</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Alexandre Espinosa Menor</bitbar.author>
# <bitbar.author.github>alexandregz</bitbar.author.github>
# <bitbar.desc>Bandwith tester, from https://speedtest.net using speedtest-cli</bitbar.desc>
# <bitbar.dependencies>speedtest-cli</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/mrPw9MV.png</bitbar.image>
#
# Dependencies: 
#   speedtest-cli (https://github.com/sivel/speedtest-cli)

# modify this path according your prefs
OUTPUT=$(~/bin/speedtest-cli --simple)
echo "$OUTPUT"
