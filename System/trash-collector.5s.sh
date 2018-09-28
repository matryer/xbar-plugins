#!/bin/bash
#
# <bitbar.title>Trash Collector</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Carlson Orozco</bitbar.author>
# <bitbar.author.github>carlsonorozco</bitbar.author.github>
# <bitbar.desc>Trash Collector is a plugin for BitBar that enables you to empty your trash.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/carlsonorozco/trash-collector/master/image.png</bitbar.image>
# <bitbar.abouturl>https://github.com/carlsonorozco/trash-collector</bitbar.abouturl>

trash_count=$(find "$HOME/.Trash/" | wc -l)
trash_count=$((trash_count-1))

trash_size=$(du -sh "$HOME/.Trash/" | xargs | head -n1 | cut -d " " -f1)

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

if [[ "$trash_size" == "0B" ]]; then
    echo "🗑 | bash='$0' param1=open terminal=false"
else
    echo "$trash_size🗑"
    echo "$trash_count items 🗑 | alternate=true"
    echo '---'
    echo "Open Trash | bash='$0' param1=open terminal=false"
    echo "Empty Trash | bash='$0' param1=empty terminal=false"
fi
