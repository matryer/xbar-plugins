#!/bin/bash
#
# Bandwith test, using speedtest-cli (https://github.com/sivel/speedtest-cli)
# Mini version! 
#
# <xbar.title>Bandwith test (minified)</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jonny Lin</xbar.author>
# <xbar.author.github>rangedsp</xbar.author.github>
# <xbar.desc>Modified mini version of Alexandre Espinosa Menor's Bandwith tester, which uses https://speedtest.net via speedtest-cli</xbar.desc>
# <xbar.dependencies>speedtest-cli</xbar.dependencies>
# <xbar.image>http://i.imgur.com/mrPw9MV.png</xbar.image>
#
# Dependencies: 
#   speedtest-cli (https://github.com/sivel/speedtest-cli)

# modify this path according your prefs

if command -v "/usr/local/bin/speedtest-cli" >/dev/null 2>&1; then
	OUTPUT=$(/usr/local/bin/speedtest-cli --simple)
else
	OUTPUT=$(~/bin/speedtest-cli --simple)
fi

result_string="${OUTPUT/Ping: /P:}"
result_string="${result_string/Download: /D:}"
result_string="${result_string/Upload: /U:}"
result_string="${result_string/ Mbit/Mb}"
result_string="${result_string/ Mbit/Mb}"
result_string="${result_string/ ms/ms}"
echo "$result_string"
