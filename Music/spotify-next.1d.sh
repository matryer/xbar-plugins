#!/bin/bash

# Switch Spotify to the next track in one click.
#
# by Aleksei Sotnikov (asotnikov.100@gmail.com) 
# metadata
# <bitbar.title>Spotify "Next track" button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Aleksei Sotnikov</bitbar.author>
# <bitbar.author.github>asotnikov</bitbar.author.github>
# <bitbar.desc>Switch Spotify to the next track in one click. Or you can easily reconfigure it for switching to the previous track.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/523Eszv.png</bitbar.image>

case "$1" in
    'previous track' | 'next track')
        osascript -e "tell application \"Spotify\" to $1"
    exit
esac

echo "▶▶ | bash='$0' param1='next track' terminal=false refresh=false"
