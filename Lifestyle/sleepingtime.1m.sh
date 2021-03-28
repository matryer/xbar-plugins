#!/bin/bash

# <xbar.title>sleepingtime</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Matteo Ferrando</xbar.author>
# <xbar.author.github>chamini2</xbar.author.github>
# <xbar.desc>Show the next sleeping cycles if we fell asleep in `falling_asleep` minutes.</xbar.desc>
# <xbar.image>http://i.imgur.com/JTqosty.png</xbar.image>

# Time in minutes to fall asleep; the mean is 15 minutes
falling_asleep=15

# Range of cycles to show in the menu
start_cycle=1
end_cycle=7

# The length of a cycle **in minutes**, standard value is 90
length=90

# Format with local time format (12 or 24 hours)
format='%H:%M'

# Display everything in local time format
echo "💤"
echo '---'

for ((cy=start_cycle; cy<=end_cycle; cy++)); do
    # Add 1 hour and 30 minutes every cycle
    h=$(((length * cy) / 60))
    m=$(((length * cy) % 60))

    if [[ $cy -gt 1 ]]; then
        str="$cy cycles:"
    else
        str="$cy cycle: "
    fi
    echo "$str $(date -v+${h}H -v+${m}M -v+${falling_asleep}M +${format})"
done
