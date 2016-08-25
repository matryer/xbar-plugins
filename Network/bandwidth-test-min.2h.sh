#!/bin/sh
#
# Bandwith test, using speedtest-cli (https://github.com/sivel/speedtest-cli)
# Mini version! 
#
# <bitbar.title>Bandwith test (minified)</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonny Lin</bitbar.author>
# <bitbar.author.github>rangedsp</bitbar.author.github>
# <bitbar.desc>Modified mini version of Alexandre Espinosa Menor's Bandwith tester, which uses https://speedtest.net via speedtest-cli</bitbar.desc>
# <bitbar.dependencies>speedtest-cli</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/mrPw9MV.png</bitbar.image>
#
# Dependencies: 
#   speedtest-cli (https://github.com/sivel/speedtest-cli)

# modify this path according your prefs
OUTPUT=$(~/bin/speedtest-cli --simple)

result_string="${OUTPUT/Ping: /P:}"
result_string="${result_string/Download: /D:}"
result_string="${result_string/Upload: /U:}"
result_string="${result_string/ Mbit/Mb}"
result_string="${result_string/ Mbit/Mb}"
result_string="${result_string/ ms/ms}"
echo "$result_string"
