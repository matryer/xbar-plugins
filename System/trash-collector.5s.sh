#!/bin/bash
#
# <bitbar.title>Trash Collector</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Carlson Orozco</bitbar.author>
# <bitbar.author.github>carlsonorozco</bitbar.author.github>
# <bitbar.desc>Trash Collector is a plugin for BitBar that enables you to empty your trash.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/carlsonorozco/trash-collector/master/image.png</bitbar.image>
# <bitbar.abouturl>https://github.com/carlsonorozco/trash-collector</bitbar.abouturl>

trash_count=$(find "$HOME/.Trash/" | wc -l)
trash_count=$((trash_count-1))

if [ "$1" = 'empty' ]; then
    osascript -e '
    tell application "Finder"
        empty the trash
    end tell'
    exit
fi

if [ "$1" = 'open' ]; then
    open "$HOME/.Trash/"
    exit
fi

if [ $((trash_count)) = 0 ]; then
    echo "🗑 | bash='$0' param1=open terminal=false"
else
    echo "$trash_count🗑 | bash='$0' param1=open terminal=false"
    echo '---'
    echo "Empty Trash | bash='$0' param1=empty terminal=false"
fi