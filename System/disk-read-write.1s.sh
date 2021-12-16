#!/usr/bin/env bash

# <xbar.title>Disk Read Write</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>cghamburg</xbar.author>
# <xbar.author.github>cghamburg</xbar.author.github>
# <xbar.desc>Shows Disk Read Write Speeds per second.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.dependencies>iostat</xbar.dependencies>

# BitBar Disk IO plugin

STAT=$(iostat -dc 2 disk0)
RATE=$(echo "$STAT"| tail -n1 | rev | cut -d ' ' -f2 | rev)
ABSOLUTE_UNIT=$(echo "$STAT"| head -n2 | tail -n1 | rev | cut -d ' ' -f2 | rev)
echo "🏃$RATE $ABSOLUTE_UNIT";
echo "---";
echo "$STAT"
