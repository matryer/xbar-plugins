#!/usr/bin/env bash

# <bitbar.title>Disk Read Write</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>cghamburg</bitbar.author>
# <bitbar.author.github>cghamburg</bitbar.author.github>
# <bitbar.desc>Shows Disk Read Write Speeds per second.</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.dependencies>iostat</bitbar.dependencies>

# BitBar Disk IO plugin

STAT=$(iostat -dc 2 disk0)
RATE=$(echo "$STAT"| tail -n1 | rev | cut -d ' ' -f2 | rev)
ABSOLUTE_UNIT=$(echo "$STAT"| head -n2 | tail -n1 | rev | cut -d ' ' -f2 | rev)
echo "🏃$RATE $ABSOLUTE_UNIT";
echo "---";
echo "$STAT"
