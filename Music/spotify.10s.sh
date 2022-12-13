#!/usr/bin/env bash
# Get current Spotify status with play/pause button
#
# by Jason Tokoph (jason@tokoph.net)
#    Marcin Swieczkowski (scatman@bu.edu)
#    Benji Encalada Mora (@benjifs)
#
# Shows current track information for Spotify
# metadata
# <xbar.title>Spotify Now Playing</xbar.title>
# <xbar.version>v1.4</xbar.version>
# <xbar.author>Jason Tokoph, Marcin S, Benji Encalada Mora</xbar.author>
# <xbar.author.github>jtokoph, m-cat, benjifs</xbar.author.github>
# <xbar.desc>Display currently playing Spotify song or podcast. Play/pause, skip forward, skip backward.</xbar.desc>
# <xbar.image>http://i.imgur.com/y1SZwfq.png</xbar.image>

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
# Comment out this line to disable cycling through track/artist
CYCLE_TRACK_ARTIST=1

# Send a series of semicolon-delimited commands to Spotify
function tellspotify() {
  commands="$(echo "$1" | tr ";" "\\n")"

  osascript -e "
            tell application \"Spotify\"
                $commands
            end tell";
}

# Handle launch spotify
if [ "$1" = 'launch' ]; then
  tellspotify 'activate'
  exit
fi
# Handle lyrics where param2 is track_title and param3 is artist
if [ "$1" = 'lyrics' ]; then
  open "https://www.musixmatch.com/search/$2 $3"
  exit
fi
# Handle play/pause/prev/next commands
first="$(echo "$1" | head -n 1 | awk '{print $1;}')"
case "$first" in
  'playpause' | 'previous' | 'next' | 'set')
    tellspotify "$1"
    exit
esac

function printdefault() {
  echo "♫"
  echo "---"
  echo "Spotify is not running"
  echo "Launch Spotify | shell='$0' param1=launch terminal=false"
}

# Truncate or cycle track and artist
function printtitle() {
  output=$1
  # Only cycle title while playing
  if [ $CYCLE_TRACK_ARTIST ] && [ "$state" = "playing" ]; then
    while [ ${#output} -gt $TRUNC_LEN ]; do
      tmp_out=${output:0:$TRUNC_LEN}
      output=${output:$TRUNC_LEN}
      echo "$state_icon $tmp_out"
    done
    if [ ${#output} -gt 0 ]; then
      # Show the last $TRUNC_LEN characters of original string
      if [ ${#1} -lt $TRUNC_LEN ]; then
        output=$1
      else
        output=${1:(-$TRUNC_LEN)}
      fi
      echo "$state_icon $output"
    fi
  else
    if [ ${#output} -gt $TRUNC_LEN ];then
      output=${output:0:$TRUNC_LEN-${#TRUNC_SUFFIX}}$TRUNC_SUFFIX
    fi
    echo "$state_icon $output | length=$((TRUNC_LEN+${#TRUNC_SUFFIX}))"
  fi
}

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

if [ "$state" = "playing" ]; then
  state_icon="▶"
else
  state_icon="❚❚"
fi

function clean_name() {
  name=$1
  name="$(echo -e "${name/ - /\\n}" | head -n 1)"
  name="$(echo -e "${name/ (Remastered/\\n}" | head -n 1)"
  echo $name
}

# Clean up track and/or album names
if [[ $CLEAN_TRACK_NAMES ]]; then
  track="$(clean_name "$track")"
fi
if [[ $CLEAN_ALBUM_NAMES ]]; then
  album="$(clean_name "$album")"
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
  printtitle "$track - $album"
  echo "---"
  echo -e "Episode: $track"
  echo -e "Podcast: $album"
elif [ "$track_type" == "SONG" ]; then
  printtitle "$track - $artist"
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
  echo -e "❚❚\\tPause | shell='$0' param1=playpause terminal=false refresh=true"
else
  echo -e "▶\\tPlay | shell='$0' param1=playpause terminal=false refresh=true"
fi
echo -e "↩\\tPrevious | shell='$0' param1='set player position to 0;previous track;play' terminal=false refresh=true"
echo -e "↪\\tNext | shell='$0' param1='next track;play' terminal=false refresh=true"
echo -e "↻\\tReplay | shell='$0' param1='set player position to 0;play' terminal=false refresh=true"

echo '---'

if [ "$track_type" == "SONG"  ]; then
  # Remove quotes in title for lyrics search
  clean_track=$(echo $track | sed 's/"//g')
  echo -e "♫\\tLyrics | shell='$0' param1=lyrics param2=\"$clean_track\" param3=\"$artist\" terminal=false"
  echo '---'
fi

echo "Open Spotify | shell='$0' param1=launch terminal=false"
