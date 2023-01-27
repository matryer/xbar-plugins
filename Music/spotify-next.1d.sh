#!/bin/bash

# Switch Spotify to the next track in one click.
#
# by Aleksei Sotnikov (asotnikov.100@gmail.com) 
# metadata
# <xbar.title>Spotify "Next track" button</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Aleksei Sotnikov</xbar.author>
# <xbar.author.github>alekseysotnikov</xbar.author.github>
# <xbar.desc>Switch Spotify to the next track in one click. Or you can easily reconfigure it for switching to the previous track.</xbar.desc>
# <xbar.image>https://i.imgur.com/523Eszv.png</xbar.image>

case "$1" in
    'previous track' | 'next track')
        osascript -e "tell application \"Spotify\" to $1"
    exit
esac

echo "▶▶ | bash='$0' param1='next track' terminal=false refresh=false"
