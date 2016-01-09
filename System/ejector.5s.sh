#!/bin/bash

# Ejector is a plugin for BitBar that enables you to eject mounted drive easily.
# BitBar plugin - https://github.com/carlsonorozco/ejector
#
# by Carlson Orozco
#
# Refresh every 5 seconds

about="About Ejector | color=red href=https://github.com/carlsonorozco/ejector"

if [ $(find /Volumes -maxdepth 1 -type d | wc -l) = "1" ]; then
    echo "⏏ | color=gray"
    echo '---'
    echo $about
    exit
fi

if [ "$1" = 'eject' ]; then
    status=$(diskutil eject /Volumes/$2 | sed -e 's/\/Volumes\///g')
    if [ "$status" = "" ]; then
        osascript -e "display notification \"Disk $2 failed to eject\" with title \"Ejector\""
    else
        osascript -e "display notification \"$status\" with title \"Ejector\""
    fi

    exit
fi

if [ "$1" = 'ejectall' ]; then
    osascript -e '
    try
        tell application "Finder"
            eject the disks
            display notification "Successfully ejected disks." with title "Ejector"
        end tell
    on error
        display notification "Unable to eject all disks." with title "Ejector"
    end try'
fi

total_drive=0

echo "⏏ | color=black"
echo '---'

ls -p /Volumes | grep / | \
{ while IFS= read -r line; do
    drive=${line%?}
    diskutil info /Volumes/$drive | grep -E 'File System Personality:|Volume Free Space:|Total Size:' | \
    { while IFS= read -r detail; do
        if [[ "$detail" =~ "Volume Free Space:" ]]
        then
            free_space=$(echo $detail | grep "Volume Free Space: " | cut -d '(' -f 1)
        elif [[ "$detail" =~ "Total Size:" ]]
        then
            total_size=$(echo $detail | grep "Total Size:" | cut -d '(' -f 1)
        fi
    done
    echo "$drive | color=black bash=$0 param1=eject param2=$drive terminal=false"
    echo "├─ $free_space"
    echo "└─ $total_size"
    }
    ((total_drive++))
done

if [ $total_drive -ge 2 ]; then
    echo "Eject All | color=red bash=$0 param1=ejectall terminal=false"
fi
}

echo '---'
echo $about