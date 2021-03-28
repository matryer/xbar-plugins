#!/bin/bash
#
# <xbar.title>Trash Collector</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Carlson Orozco</xbar.author>
# <xbar.author.github>carlsonorozco</xbar.author.github>
# <xbar.desc>Trash Collector is a plugin for BitBar that enables you to empty your trash.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/carlsonorozco/trash-collector/master/image.png</xbar.image>
# <xbar.abouturl>https://github.com/carlsonorozco/trash-collector</xbar.abouturl>

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
