#!/bin/bash
#
# <bitbar.title>Handoff Toggle</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Martin Schilliger</bitbar.author>
# <bitbar.author.github>martinschilliger</bitbar.author.github>
# <bitbar.desc>Simple toggle for macOS Handoff. Great if there is someone using your second mac or iPad and you don't wanna paste everything the other person copies.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/leZPz8Y.jpg</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/martinschilliger/</bitbar.abouturl>

#UUID=$(/usr/sbin/system_profiler SPHardwareDataType | grep "Hardware UUID" | cut -c22-57)
#PREF_FILE="${HOME}/Library/Preferences/ByHost/com.apple.coreservices.useractivityd.${UUID}.plist"
PREF_FILE="${HOME}/Library/Preferences/ByHost/com.apple.coreservices.useractivityd.plist"

ICON_ENABLED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAB70lEQVR4AWJwL/ABtFsOMHfDURSft2iOjdnlvMUv1oJ5wWej/Z5ZjcEQc2YwxWOcOZoRzIid7Jzx2Uza5Jde9d7zbvuSf0NxBbgCXAEpjizLayRJega+i6L4pZqwJ3sLgrA6n4BD4BMeOAcuVJlz7M0ZOQVA4TEUnK/VutmbM/Jt4CgKLtVKAHtzRkUCcE1AnYSVbgbrCW1VVUXmai4Aw7agxgAnFEW5T2CfBAnmai6Ag0ZGRoKGYeyJRCKdhPbo6GgAAuL12ICpaZrtOE5XMrqum8hF67GBvV6vdx+uvmR8Pp/DXD02sJ+/FkM7ksFWDOT21UNAdGhoKGzb9q5k+F0gF6vHK/APDg6GLMvamczw8HCA4uqxgUBTCmCsLAGbNm2ahgcn/xFwBFws0MSXSwBykXzPsjdn0F68ePEUj8czlUEJ10GIuA4+wP6B+23c74I7hD64BZvx1z09PSY+vB3JMIaad6xhbRZu/un9AVyBncDs+X/XOhfORgR3wu7nh8ZV/yEIP0xQExUE4Ulvb28sXUBfX18UtTxL2MCBve8v8A32Ab1gG88duM8o60TE94y/nC9dAGPIWTU/kkF9f3t7+zFd14c1TRshtBnj9mouYNmyZbN4wlEU5TNW+xa8ps3YihUrZrqn4pYT4Ar4CW6NezCnH1ZyAAAAAElFTkSuQmCC";
ICON_DISABLED="iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAB9ElEQVR4Ae2Wg69cYRDFa4a1bQe1bdtuVDtYe/eytsK6caPun7b9nfLZ3z7dJCffeM7MZZeNV7e1KgICAYFKSkAgm82Oyufzl3K53B3k2y0J1VRt9aiVgGVZGwuFwq1MJrOfwAMtCdVUbfWolQABW23b3m9q3aqtHrUSIGALqzoo2QRUWz2aRaBUKnXlWk5krTNTqdQ0QbLjOBPkM06AZotY4yFwmaZ5gZwr2A/JZ54AjRKJxFhNS3xfQTK20el0+qBxAvgPc0dPqGrHNk53u3ECNDpWGwH5jBNgzce5Cecx7bK/NvQN3BPjOc0T0Jq1Ac4vuvOJV064bJeAJnv+EBgFvgnFYrFH2Qn8KfgdPSW57ASIOwMuoL8jby6P4fgmEYhEIj1Atz8BW7iZ6izCjbZbNyCxH6X7vj8Y/YfrulPx7a8rV7XVQ/L+/fu7f/jwobuajtBdzHmUQjfBXXCaKc/o/AtiTv2xP+GcXrW4bPhe/I2tCnzKV231OEK/1TxRA5WsLfSD3VgC52BciHP5X5C4gnOlQMwqkh8gT65hvZPxPbcsay111iGv/wvy1qhOMplcgD4bjGJzvZv0R6TrrFdxVbtsmsz4L9mf5345Uw5jouGCZE3I9rYYJ8A6+0Pghud5L2n8GDyULJt8wV9xQKDdEfgJKdr5nu4nT60AAAAASUVORK5CYII=";

function get_handoff_state {
    state=$(defaults read "${PREF_FILE}" ActivityAdvertisingAllowed)
}

function toggle_handoff {
    defaults write "${PREF_FILE}" ActivityAdvertisingAllowed "$1"
    defaults write "${PREF_FILE}" ActivityReceivingAllowed "$1"
}

if [[ "$1" = "enable" ]]; then
    toggle_handoff 1;
    exit
fi

if [[ "$1" = "disable" ]]; then
    toggle_handoff 0;
    exit
fi

get_handoff_state;
if [ "$state" = 1 ]; then
	echo "| templateImage=${ICON_ENABLED} bash='$0' param1=disable terminal=false refresh=true emojize=false";
    echo "---"
	exit
else
	echo "| templateImage=${ICON_DISABLED} bash='$0' param1=enable terminal=false refresh=true emojize=false";
    echo "---"
	exit
fi
