#!/bin/bash
# <bitbar.title>Battery Cycles</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Maxime Bertheau</bitbar.author>
# <bitbar.author.github>maxoumime</bitbar.author.github>
# <bitbar.desc>Get your battery cycles and condition on the menu bar !</bitbar.desc>
# <bitbar.image>https://nothingreally.botler.me/bitbar.battery-cycles.png</bitbar.image>

# Get cycles number
cycles=$(system_profiler SPPowerDataType | grep "Cycle Count" | awk '{print $3}')

# If you want to change the emoticon before the number, here are some
#echo "🔋 $cycles"
#echo "♾ $cycles"
#echo "♽ $cycles"

echo "♾ $cycles"

echo "---"

# 1000 cycles maximum
# Based on the cycles max number provided by Apple for all Macbooks after 2009
# No way to get the Macbook age from script directly

color=green
if [ "$cycles" -gt 1000 ]; then
    color=red
elif [ "$cycles" -gt 750 ]; then
    color=orange
fi

echo "Cycles: $cycles / 1000 | color=$color href='https://support.apple.com/en-us/HT201585'"

# Get battery condition
condition=$(system_profiler SPPowerDataType | grep "Condition" | sed -e 's/^.*: //')

color=green
if [ "$condition" != "Normal" ]; then
    color=red
fi

echo "Battery condition: $condition | color=$color"
