#!/bin/bash

# Display current Spotify status with play/pause/skip buttons
#
# by Igor Borges (igor@borges.me)
# heavily inspired by Jason Tokoph's spotify.10s.sh
#
# Shows current track information from Spotify
# Make sure you have spotctl installed and configured.

# metadata
# <bitbar.title>Spotify Now Playing (via spotctl)</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Igor Borges</bitbar.author>
# <bitbar.author.github>Igor1201</bitbar.author.github>
# <bitbar.desc>Display currently playing Spotify song using spotctl. Play/pause, skip forward, skip backward.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/zAGFguy.png</bitbar.image>

# That's "$(brew --prefix)/bin" path, but we don't have brew on the $PATH to run it ¯\_(ツ)_/¯
export PATH="$PATH:/usr/local/bin"

function shorten {
  length=20
  ellipsis='…'
  read -r string
  echo "$string" | sed -E "s/(.{${length}}).*$/\\1${ellipsis}/"
}

case "$1" in
  'play' | 'pause' | 'prev' | 'next')
    spotctl "$1"
    sleep 1
    exit
esac

state=$(spotctl status | sed -n 1p | grep -o playing || echo paused)

if [ "$state" = "playing" ]; then
  status=$(spotctl status)
  track=$(echo "$status" | sed -n 4p | cut -c 8-)
  artist=$(echo "$status" | sed -n 2p | cut -c 9-)
  album=$(echo "$status" | sed -n 3p | cut -c 8-)
  shortened_track=$(echo "$track" | shorten)
  shortened_artist=$(echo "$artist" | shorten)

  echo "▶ $shortened_track - $shortened_artist"
  echo "---"
  echo "Track: $track | color=#666666"
  echo "Artist: $artist | color=#666666"
  echo "Album: $album | color=#666666"
  echo '---'
  echo "❚❚ Pause | bash='$0' param1=pause terminal=false refresh=true"
  echo "↩ Previous | bash='$0' param1=prev terminal=false refresh=true"
  echo "↪ Next | bash='$0' param1=next terminal=false refresh=true"
else
  echo "♫"
  echo '---'
  echo "▶ Play | bash='$0' param1=play terminal=false refresh=true"
fi
