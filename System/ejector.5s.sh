#!/bin/bash
#
# <bitbar.title>Ejector</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Carlson Orozco</bitbar.author>
# <bitbar.author.github>carlsonorozco</bitbar.author.github>
# <bitbar.desc>Ejector is a plugin for BitBar that enables you to eject mounted drive easily.</bitbar.desc>

about="About Ejector | color=red href=https://github.com/carlsonorozco/ejector"

if [ "$1" = 'eject' ]; then
    status=$(diskutil eject /Volumes/"$2" | sed -e 's/\/Volumes\///g')
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

    exit
fi

total_drive=$(find /Volumes -maxdepth 1 -type d | wc -l | tr -s '[:space:]' )

if [ $((total_drive)) = 1 ]; then
    echo "⏏ | color=gray"
    echo '---'
    echo "$about"
    exit
fi

echo "⏏ | color=black"
echo '---'

df -Hl | grep /Volumes/ |
while IFS= read -r line; do
    drive=${line#*/Volumes/}

    diskutil info /Volumes/"$drive" | grep -E 'Volume Free Space:|Total Size:' | \
    { while IFS= read -r detail; do
        if [[ "$detail" =~ "Volume Free Space:" ]]
        then
            free_space=$(echo "$detail" | grep "Volume Free Space:" | cut -d '(' -f 1)
        elif [[ "$detail" =~ "Total Size:" ]]
        then
            total_size=$(echo "$detail" | grep "Total Size:" | cut -d '(' -f 1)
        fi
    done
    echo "$drive | color=black bash=$0 param1=eject param2=$drive terminal=false"
    echo "├─ $free_space"
    echo "└─ $total_size"
    }
done

if [ $((total_drive)) -ge 3 ]; then
    echo "Eject All | color=red bash=$0 param1=ejectall terminal=false"
fi

echo '---'
echo "$about"