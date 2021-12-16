#!/bin/bash
#
# <xbar.title>Power Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jeff Beadles</xbar.author>
# <xbar.author.github>jeffbeadles</xbar.author.github>
# <xbar.desc>Shows power source, current mode and settings</xbar.desc>
#
export PATH="/bin:/usr/bin:$PATH"

# AC or Battery?
pmset -g cap | sed -ne 's/^Capabilities for \(.*\) Power:$/\1/p'
echo "---"
# Power setting details
pmset -g live | \
   sed -e '1,3d' \
       -e 's/Currently in use.*$/Current power settings/' \
       -e 's/$/ | font=Courier color=black size=12/'

exit 0
#eof
