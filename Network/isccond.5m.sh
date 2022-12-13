#!/bin/bash

# disabling a linter check which doesn't like unquoted tr
# shellcheck disable=SC2060

# <xbar.title>ISC Condition Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>panzertime</xbar.author>
# <xbar.author.github>panzertime</xbar.author.github>
# <xbar.desc>Gets ISC's condition code and displays it</xbar.desc>
# <xbar.image></xbar.image>
# <xbar.dependencies></xbar.dependencies>
# <xbar.abouturl>https://github.com/panzertime/bitbar-plugins</xbar.abouturl>

color=$(curl --silent -m 1 https://isc.sans.edu/infocon.txt | tr [a-z] [A-Z])
echo "ISC Cond ${color} | color=${color}"
