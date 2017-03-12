#!/bin/bash
#
# <bitbar.title>Alarm Clock</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Carlson Orozco</bitbar.author>
# <bitbar.author.github>carlsonorozco</bitbar.author.github>
# <bitbar.desc>Alarm Clock is a plugin for BitBar that notifies/make a sound at a specific time.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/carlsonorozco/alarmclock/master/image.png</bitbar.image>
# <bitbar.abouturl>https://github.com/carlsonorozco/alarmclock</bitbar.abouturl>

# Set Alarm
if [ "$1" = 'set' ]; then
    alarm="$(osascript -e 'Tell application "System Events" to display dialog "Enter Alarm in 24 Hour Time format:" default answer ""' -e 'text returned of result' 2>/dev/null)"
    # shellcheck disable=SC2181
    if [ $? -ne 0 ]; then
        # Cancelled
        exit
    elif [ -z "$alarm" ]; then
        osascript -e 'Tell application "System Events" to display alert "Alarm not set" as warning'
        exit
    elif [[ ! $alarm =~ ^([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]$ ]]; then
        osascript -e 'Tell application "System Events" to display alert "Alarm not set. Invalid time format" as warning'
        exit
    fi

    echo "$alarm" >> /tmp/alarmclock.data
    exit
fi

# Remove Alarm
if [ "$1" = 'remove' ]; then
    touch /tmp/alarmclock_tmp.data
    while IFS= read -r line; do
        if [ "$2" != "$line" ]; then
            echo "$line" >> /tmp/alarmclock_tmp.data
        fi
    done </tmp/alarmclock.data
    mv /tmp/alarmclock_tmp.data /tmp/alarmclock.data
    exit
fi

# Stop Alarm
if [ "$1" = 'stop' ]; then
    rm /tmp/alarmclock_trigger.data
    exit
fi

# Create alarmclock.data if not exist
if [ ! -f '/tmp/alarmclock.data' ]; then
    touch '/tmp/alarmclock.data'
fi

# Count all alarms
total_alarms=$(grep -c ':' /tmp/alarmclock.data)

# Header Display
if [ $((total_alarms)) -gt 0 ]; then
    if [ ! -f '/tmp/alarmclock_trigger.data' ] ; then
        echo "$total_alarms⏰"
    else
        echo "$total_alarms⏰ | color=red"
    fi
    echo '---'
else
    echo "⏰"
fi

# Parse alarms
while IFS= read -r line; do
    echo "$line | color=red bash='$0' param1=remove param2=$line terminal=false"
    now=$(date +%R)
    if [ "$line" == "$now" ] && [ "$(date +%S)" -le '03' ] && [ ! -f '/tmp/alarmclock_trigger.data' ]; then
        echo "$now" > '/tmp/alarmclock_trigger.data'
    fi
done </tmp/alarmclock.data

# Set new alarm
echo '---'
echo "Set Alarm | color=green bash='$0' param1=set terminal=false"

# Trigger alarm
if [ -f '/tmp/alarmclock_trigger.data' ]; then
    echo "Stop Alarm | color=red bash='$0' param1=stop terminal=false"
    afplay /System/Library/Sounds/Tink.aiff
    afplay /System/Library/Sounds/Tink.aiff
    afplay /System/Library/Sounds/Tink.aiff
    afplay /System/Library/Sounds/Tink.aiff
fi