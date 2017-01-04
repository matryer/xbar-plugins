#!/bin/bash

# Get current Spotify status with play/pause button
#
# by Jason Tokoph (jason@tokoph.net)
#
# Shows current track information from spotify
# 10 second refresh might be a little too quick. Tweak to your liking.

# metadata
# <bitbar.title>Spotify Now Playing</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Jason Tokoph</bitbar.author>
# <bitbar.author.github>jtokoph</bitbar.author.github>
# <bitbar.desc>Display currently playing Spotify song. Play/pause, skip forward, skip backward.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/y1SZwfq.png</bitbar.image>

function tellspotify() {
  osascript -e "tell application \"Spotify\" to $1"
}

if [ "$1" = 'launch' ]; then
  tellspotify 'activate'
  exit
fi

if [ "$(osascript -e 'application "Spotify" is running')" = "false" ]; then
  echo "♫"
  echo "---"
  echo "Spotify is not running"
  echo "Launch Spotify | bash='$0' param1=launch terminal=false"
  exit
fi

case "$1" in
  'playpause' | 'previous track' | 'next track')
    tellspotify "$1"
    exit
esac

state=$(tellspotify 'player state as string');

if [ "$state" = "playing" ]; then
  state_icon="▶"
else
  state_icon="❚❚"
fi

suffix="..."
trunc_length=20
track=$(tellspotify 'name of current track as string');
truncated_track=$track
if [ ${#track} -gt $trunc_length ];then
  truncated_track=${track:0:$trunc_length-${#suffic}}$suffix
fi
artist=$(tellspotify 'artist of current track as string');
truncated_artist=$artist
if [ ${#artist} -gt $trunc_length ];then
  truncated_artist=${artist:0:$trunc_length-${#suffic}}$suffix
fi
album=$(tellspotify 'album of current track as string');

echo "$state_icon $truncated_track - $truncated_artist"
echo "---"

echo "Track: $track | color=#333333"
echo "Artist: $artist | color=#333333"
echo "Album: $album | color=#333333"

echo '---'

if [ "$state" = "playing" ]; then
  echo "⏸ Pause | bash='$0' param1=playpause terminal=false"
  echo "⏮ Previous | bash='$0' param1='previous track' terminal=false refresh=true"
  echo "⏭ Next | bash='$0' param1='next track' terminal=false refresh=true"
else
  echo "▶️ Play | bash='$0' param1=playpause terminal=false"
fi

echo '---'
echo "Open Spotify | bash='$0' param1=launch terminal=false"
