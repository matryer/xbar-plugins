#!/bin/sh
#
# Bandwith test, using speedtest-cli (https://github.com/sivel/speedtest-cli) 
#
# <xbar.title>Bandwith test</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alexandre Espinosa Menor</xbar.author>
# <xbar.author.github>alexandregz</xbar.author.github>
# <xbar.desc>Bandwith tester, from https://speedtest.net using speedtest-cli</xbar.desc>
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

echo "$OUTPUT"
