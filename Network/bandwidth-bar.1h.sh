#!/bin/sh
#
# Bandwidth Bar, using speedtest-cli (https://github.com/sivel/speedtest-cli)
# Based on Alexandre Espinosa Menor's Bandwith test
#
# <bitbar.title>Bandwidth Bar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Andrew Strozier</bitbar.author>
# <bitbar.author.github>astroz</bitbar.author.github>
# <bitbar.desc>Customizable bandwidth stats using speedtest-cli.</bitbar.desc>
# <bitbar.dependencies>speedtest-cli</bitbar.dependencies>
#
# Dependencies:
#   speedtest-cli (https://github.com/sivel/speedtest-cli)
#
# Customization:
#   Removing / adding "#?" to the start of a code line will enable / disable the option.
#   By default, only SIMPLE_DOWNLOAD and download speed on-bar are enabled.
#   *The $OUTPUT variable should be echoed at the end, regardless of customization.

# Change this path to the install location of speedtest_cli. (Required)
cd /Library/Python/2.7/site-packages

# Simple mode only outputs the ping, download, and upload results. (Required)
OUTPUT=$(python speedtest_cli.py --simple)

# These variables replace full words on the bar with arrows.
#?SIMPLE_PING="'s/Ping:/●/g'"
SIMPLE_DOWNLOAD="'s/Download:/▼/g'"
#?SIMPLE_UPLOAD="'s/Upload:/▲/g'"

# This line shows ping on the bar.
#?echo "$OUTPUT" | eval sed -n "/Ping:/p" | eval sed "$SIMPLE_PING"

# This line shows download speed on the bar.
echo "$OUTPUT" | eval sed -n '/Download:/p' | eval sed "$SIMPLE_DOWNLOAD"

# This line shows upload speed on the bar.
#?echo "$OUTPUT" | eval sed -n "/Upload:/p" | eval sed "$SIMPLE_UPLOAD"

# Outputs below this line will only display when BB is expanded.
echo "---"

# The full output of speedtest_cli. (Required*)
echo "$OUTPUT"
