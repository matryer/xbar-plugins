#!/bin/bash

# Get current Spotify status with play/pause button
#
# by Jason Tokoph (jason@tokoph.net)
#    Marcin Swieczkowski (scatman@bu.edu)
#    Benji Encalada Mora (@benjifs)
#
# Shows current track information for Spotify

# metadata
# <bitbar.title>Spotify Now Playing</bitbar.title>
# <bitbar.version>v1.3</bitbar.version>
# <bitbar.author>Jason Tokoph, Marcin S, Benji Encalada Mora</bitbar.author>
# <bitbar.author.github>jtokoph</bitbar.author.github>
# <bitbar.desc>Display currently playing Spotify song and podcast. Play/pause, skip forward, skip backward.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/y1SZwfq.png</bitbar.image>

# Comment the following line to disable showing times.
SHOW_TIME=1

# By default we remove anything after " - ", as it usually is stuff like
# "Remastered", "Single Version", or other garbage that Spotify likes to
# include.
#
# Comment out this line if you want the full track names.
CLEAN_TRACK_NAMES=1
# Comment out this line if you want the full album names.
CLEAN_ALBUM_NAMES=1

# The length of a track/artist name after which to truncate.
TRUNC_LEN=18
# String used when replacing truncated text.
TRUNC_SUFFIX="..."

# Send a series of semicolon-delimited commands to Spotify
function tellspotify() {
  commands="$(echo "$1" | tr ";" "\\n")"

  osascript -e "
            tell application \"Spotify\"
                $commands
            end tell";
}

function printdefault() {
  echo "♫"
  echo "---"
  echo "Spotify is not running"
  echo "Launch Spotify | bash='$0' param1=launch terminal=false"
}

## Handle early-return cases

if [ "$1" = 'launch' ]; then
  tellspotify 'activate'
  exit
fi

first="$(echo "$1" | head -n 1 | awk '{print $1;}')"
case "$first" in
  'playpause' | 'previous' | 'next' | 'set')
    tellspotify "$1"
    exit
esac

# Check if Spotify is running or if `state` is "stopped"
if [ "$(osascript -e 'application "Spotify" is running')" = "false" ]; then
  printdefault
  exit
fi
state=$(tellspotify 'player state as string');
if [ "$state" == "stopped" ]; then
  printdefault
  exit
fi

## Get Spotify info
id=$(tellspotify 'id of current track as string');
track=$(tellspotify 'name of current track as string');
artist=$(tellspotify 'artist of current track as string');
album=$(tellspotify 'album of current track as string');

## Check track type
if [[ "$id" == *":episode:"* ]]; then
	track_type="PODCAST"
	unset CLEAN_TRACK_NAMES
else
	track_type="SONG"
fi

# Handle last early-return case (needed $track and $artist to look up lyrics).
if [ "$1" = 'lyrics' ]; then
  open "https://www.musixmatch.com/search/$track $artist"
  exit
fi

if [ "$state" = "playing" ]; then
  state_icon="▶"
else
  state_icon="❚❚"
fi

# Clean up track and/or album names
if [[ $CLEAN_TRACK_NAMES ]]; then
  track="$(echo -e "${track/ - /\\n}" | head -n 1)"
  track="$(echo -e "${track/ (Remastered/\\n}" | head -n 1)"
fi
if [[ $CLEAN_ALBUM_NAMES ]]; then
  album="$(echo -e "${album/ - /\\n}" | head -n 1)"
  album="$(echo -e "${album/ (Remastered/\\n}" | head -n 1)"
fi

## Truncate track and artist
trunc_track=$track
if [ ${#trunc_track} -gt $TRUNC_LEN ];then
  trunc_track=${trunc_track:0:$TRUNC_LEN-${#TRUNC_SUFFIX}}$TRUNC_SUFFIX
fi

trunc_artist=$artist
if [ ${#trunc_artist} -gt $TRUNC_LEN ];then
  trunc_artist=${trunc_artist:0:$TRUNC_LEN-${#TRUNC_SUFFIX}}$TRUNC_SUFFIX
fi

# Get position and duration of track
if [[ $SHOW_TIME ]]; then
  position=$(osascript -e \
                       "tell application \"Spotify\"
                            set pos_sec to player position
                            set time_min to (pos_sec / 60 div 1) as text
                            set raw_sec to (pos_sec mod 60 div 1) as text
                            if length of raw_sec is greater than 1 then
                                set time_sec to raw_sec
                            else
                                set time_sec to \"0\" & raw_sec
                            end if
                            return time_min as text & \":\" & time_sec as text
                        end tell");
  duration=$(osascript -e \
                       "tell application \"Spotify\"
                            set total_sec to (duration of current track / 1000) as text
                            set time_min to (total_sec / 60 div 1) as text
                            set raw_sec to (total_sec mod 60 div 1) as text
                            if length of raw_sec is greater than 1 then
                                set time_sec to raw_sec
                            else
                                set time_sec to \"0\" & raw_sec
                            end if
                            return time_min as text & \":\" & time_sec as text
                        end tell");
fi

## Print the display
if [ "$track_type" == "PODCAST" ]; then
	echo "$state_icon $track - $album | length=$TRUNC_LEN"
	echo "---"
	echo -e "Episode: $track"
	echo -e "Podcast: $album"
elif [ "$track_type" == "SONG" ]; then
	echo "$state_icon $track - $artist | length=$TRUNC_LEN"
	echo "---"
	echo -e "Track:\\t$track"
	echo -e "Artist:\\t$artist"
	echo -e "Album:\\t$album"
fi
echo "---"

if [[ $SHOW_TIME ]]; then
  echo "${position} / ${duration}"
  echo '---'
fi

if [ "$state" = "playing" ]; then
  echo -e "❚❚\\tPause | bash='$0' param1=playpause terminal=false refresh=true"
else
  echo -e "▶\\tPlay | bash='$0' param1=playpause terminal=false refresh=true"
fi
echo -e "↩\\tPrevious | bash='$0' param1='set player position to 0;previous track;play' terminal=false refresh=true"
echo -e "↪\\tNext | bash='$0' param1='next track;play' terminal=false refresh=true"
echo -e "↻\\tReplay | bash = '$0' param1='set player position to 0;play' terminal=false refresh=true"

echo '---'

if [ "$track_type" == "SONG"  ]; then
	echo -e "♫\\tLyrics | bash='$0' param1=lyrics terminal=false"
  echo '---'
fi

echo "Open Spotify | bash='$0' param1=launch terminal=false"
