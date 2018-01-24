#!/bin/bash

# disabling a linter check which doesn't like unquoted tr
# shellcheck disable=SC2060

# <bitbar.title>ISC Condition Monitor</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>panzertime</bitbar.author>
# <bitbar.author.github>panzertime</bitbar.author.github>
# <bitbar.desc>Gets ISC's condition code and displays it</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl>https://github.com/panzertime/bitbar-plugins</bitbar.abouturl>

color=$(curl --silent -m 1 https://isc.sans.edu/infocon.txt | tr [a-z] [A-Z])
echo "ISC Cond ${color} | color=${color}"
