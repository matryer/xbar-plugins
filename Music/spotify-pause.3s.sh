#!/bin/bash

# Play/pause Spotify button
#
# by Aleksei Sotnikov (asotnikov.100@gmail.com) 
# metadata
# <bitbar.title>Spotify "Next track" button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Aleksei Sotnikov</bitbar.author>
# <bitbar.author.github>alekseysotnikov</bitbar.author.github>
# <bitbar.desc>Play or pause Spotify in one click.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/kLaa2uz.png</bitbar.image>

function tellspotify() {
  osascript -e "tell application \"Spotify\" to $1"
}

case "$1" in
  'playpause' )
    tellspotify "$1"
    exit
esac

state=$(tellspotify 'player state as string');

if [ "$state" = "playing" ]; then
  state_icon="❚❚"
else
  state_icon="▶"
fi

echo "$state_icon | bash='$0' param1=playpause terminal=false refresh=false"

