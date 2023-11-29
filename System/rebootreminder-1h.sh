#!/bin/bash

# <xbar.title>reboot reminder</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Michael Kwun</xbar.author>
# <xbar.author.github>lawtalker</xbar.author.github>
# <xbar.desc>Days of uptime, in red after specified number of days.</xbar.desc>
# <xbar.image>https://i.imgur.com/mFjf0oA.jpeg</xbar.image>
# <xbar.abouturl>https://github.com/lawtalker/rebootreminder</xbar.abouturl>
# <xbar.var>number(VAR_RED_DAY=7): Red starting on this day</xbar.var>
# <xbar.var>number(VAR_GRAY_ZERO=190): Day 0 gray level</xbar.var>

# based on Matteo Ferrando's uptime plugin 
# which is at https://xbarapp.com/docs/plugins/System/uptime.1m.sh.html

# the rewritten plugin just show days, in gray until 
# in red starting on a specified day

uptime | sed 's/^ *//g' | awk -F'[ ,:\t\n]+' -v RED=$VAR_RED_DAY -v GRAY=$VAR_GRAY_ZERO '{

    if (GRAY > 255) {
        GRAY=255
    }
    
    if (substr($5,0,1) == "d") {
        D = $4
    }
    else {
        D = 0
    }
    
    if (D == 1) {
        UNIT = "day"
    }
    else {
        UNIT = "days"
    }
    
    if (D < RED) {
        R = int( GRAY - GRAY/(RED-1) * D )
        GB = R
    }
    else {
        R = 255
        GB = 0
    }
    
    printf "[ ↑ %d %s ] | color=#%02x%02x%02x | size=12\n", D, UNIT, R, GB, GB
}'

echo "---"
echo "plugin by Michael Kwun | href=https://github.com/lawtalker"
echo "inspired by uptime by Matteo Ferrando | href=https://xbarapp.com/docs/plugins/System/uptime.1m.sh.html"
