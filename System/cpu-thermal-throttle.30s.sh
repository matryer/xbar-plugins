#!/bin/bash

# <xbar.title>CPU thermal throttling</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Sampo Juustila</xbar.author>
# <xbar.author.github>plaa</xbar.author.github>
# <xbar.desc>Displays the current CPU thermal throttling speed (using `pmset -g therm`).</xbar.desc>

OUTPUT="$(pmset -g therm)"
SCHEDLIMIT="$(echo "$OUTPUT" | grep CPU_Scheduler_Limit | cut -d= -f2)"
SPEEDLIMIT="$(echo "$OUTPUT" | grep CPU_Speed_Limit | cut -d= -f2)"
AVAILCPU="$(echo "$OUTPUT" | grep CPU_Available_CPUs | cut -d= -f2)"
TOTAL=$(($SCHEDLIMIT * $SPEEDLIMIT / 100))

if [ "$TOTAL" -ge 80 ]; then
  SYMBOL="🌡"
else
  SYMBOL="🔥"
fi

cat <<EOF
$SYMBOL$TOTAL%
---
CPU_Speed_Limit $SPEEDLIMIT%
CPU_Scheduler_Limit $SCHEDLIMIT%
CPU_Available_CPUs $AVAILCPU
EOF
