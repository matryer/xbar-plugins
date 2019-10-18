#!/bin/bash

# Play/pause Spotify button
#
# by Aleksei Sotnikov (asotnikov.100@gmail.com) 
# thanks for contribution by Doug Cotler (dcotler@seas.upenn.edu)
# metadata
# <bitbar.title>Spotify play/pause button</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Aleksei Sotnikov</bitbar.author>
# <bitbar.author.github>alekseysotnikov</bitbar.author.github>
# <bitbar.desc>Play or pause Spotify in one click</bitbar.desc>
# <bitbar.image>https://i.imgur.com/kLaa2uz.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>

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