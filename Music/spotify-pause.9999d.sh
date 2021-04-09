#!/bin/bash

# Play/pause Spotify button
#
# by Aleksei Sotnikov (asotnikov.100@gmail.com) 
# thanks for contribution by Doug Cotler (dcotler@seas.upenn.edu)
# metadata
# <xbar.title>Spotify play/pause button</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Aleksei Sotnikov</xbar.author>
# <xbar.author.github>alekseysotnikov</xbar.author.github>
# <xbar.desc>Play or pause Spotify in one click</xbar.desc>
# <xbar.image>https://i.imgur.com/kLaa2uz.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

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