#!/bin/bash

# Get current iTunes status with play/pause button
#
# based on Spotify script by Jason Tokoph (jason@tokoph.net), tweaked by Dan
# Turkel (daturkel@gmail.com) 
#
# Shows current track information from iTunes
# 10 second refresh might be a little too quick. Tweak to your liking.

# metadata
# <bitbar.title>iTunes Now Playing</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dan Turkel, Jason Tokoph</bitbar.author>
# <bitbar.author.github>daturkel</bitbar.author.github>
# <bitbar.desc>Display currently playing iTunes song. Play/pause, skip forward, skip backward.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/1Q81FL4.png</bitbar.image>

if [ "$1" = 'launch' ]; then
  osascript -e 'tell application "iTunes" to activate'
  exit
fi

if [ "$(osascript -e 'application "iTunes" is running')" = "false" ]; then
  echo "♫ | color=green size=10"
  echo "---"
  echo "iTunes is not running"
  echo "Launch iTunes | bash=$0 param1=launch terminal=false"
  exit
fi

if [ "$1" = 'playpause' ]; then
  osascript -e 'tell application "iTunes" to playpause'
  exit
fi

if [ "$1" = 'previous' ]; then
  osascript -e 'tell application "iTunes" to previous track'
  exit
fi

if [ "$1" = 'next' ]; then
  osascript -e 'tell application "iTunes" to next track';
  exit
fi

state=$(osascript -e 'tell application "iTunes" to player state as string');

track=$(osascript -e'
try
tell application "iTunes" to name of current track as string
on error errText
  "no track selected"
end try
');

artist=$(osascript -e'
try
	tell application "iTunes" to artist of current track as string
on error errText
    ""
end try
');


album=$(osascript -e'
try
	tell application "iTunes" to album of current track as string
on error errText
    ""
end try
');

if [ "$state" = "playing" ]; then
  state_icon="▶️"
  
else
  state_icon="⏸"
fi

echo "$state_icon $track - $artist | color=green size=10"
echo "---"

case "$0" in
  *\ * )
   echo "Your script path | color=#ff0000"
   echo "($0) | color=#ff0000"
   echo "has a space in it, which BitBar does not support. | color=#ff0000"
   echo "Play/Pause/Next/Previous buttons will not work. | color=#ff0000"
  ;;
esac

echo "Track: $track |  color=green size=10"
echo "Artist: $artist | color=green size=10"
echo "Album: $album | color=green size=10"

echo '---'

if [ "$state" = "playing" ]; then
  echo "Pause | bash=$0 param1=playpause terminal=false refresh=true color=green size=10"
  echo "Previous | bash=$0 param1=previous terminal=false refresh=true color=green size=10"
  echo "Next | bash=$0 param1=next terminal=false refresh=true color=green size=10"
else
  echo "Play | bash=$0 param1=playpause terminal=false refresh=true color=green size=10"
fi
