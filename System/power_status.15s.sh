#!/bin/bash
#
# <bitbar.title>Power Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jeff Beadles</bitbar.author>
# <bitbar.author.github>jeffbeadles</bitbar.author.github>
# <bitbar.desc>Shows power source, current mode and settings</bitbar.desc>
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
