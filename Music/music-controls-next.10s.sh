#!/bin/bash

# Plays the next track in cmus, iTunes, Music, Spotify or pianobar.
#
# Special thanks to Google for providing the open-source icons: https://github.com/google/material-design-icons
# and to mcchrish and alekseysotnikov for their helpful existing BitBar scripts
#
# metadata
# <bitbar.title>Music Controls - Next Track Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Plays the next track in cmus, iTunes, Music, Spotify or pianobar.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/sebbas/music-controls-bitbar/master/music-controls-screenshot.png</bitbar.image>
# <bitbar.abouturl>http://github.com/sebbas/music-controls-bitbar</bitbar.abouturl>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
export LC_CTYPE="UTF-8"

pianobar_ctlfile="$HOME/.config/pianobar/ctl"

NONE="none"
CMUS="cmus"
ITUNES="iTunes"
MUSIC="Music"
SPOTIFY="Spotify"
PIANOBAR="pianobar"

next_icon_light="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAXtJREFUaAXtWDtOA0EMjRAgKppIaVKnpKDNCahzC25AbsAJOEOa1JyAloIydSpEkyZFJBSekSyNrMx6Bu8qjPRGGs069vOzn/PZzWjERQWoABWgAlSAClABKlCjwArBtzUAJ/YIf7qd8LhbyLbY83iq3wxp8XI9+FLCbzA9Y18GGTWfnsF0PlyJ9HwHZObDshGaR89sYF8OJUrPPZI//pEgzSPXgy9LmNqvYB9XVpDiz96AFPCJ/VDRxL9rQAt6QRM3BY1ovJ4FkFiIEpWcG1DdOXQ2jxMed1tCzz6A8gn7IkNt8Zmw/l62hKX2G0qYnijD4k+E9PuSJayxdyhlYcqxeOPuNnNj7UbFvNcxeBxtFSu1m30LyYd4iZ2bthUgLrGTwRJ22fI1el+ZzwmPu7sKTn3N/pB9QaNmbyXkZm5SOeR0anI9+LKEYjd9O/2BBpp8oNFHyqvgzO1Eg+l8uBA2/VC/RgNN/63iz4gRVIAKUAEqQAWoABVoQ4Efj0sWuAZStX4AAAAASUVORK5CYII="
next_icon_dark="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAYVJREFUaAXtWDFOAzEQPCGCUtEgpaGmpEjLC6j5BT+AH+QFvIGGmhfQUlBSUyGaNCmQ0DEX2ZIzOnnXXoNkaSNZ9mZnduzZXHKXYfCXO+AOuAPugDvgDrgD7oDagXEcHzFO1QQBiFoHLwFuTwe1D8xX9mrDcLB7BC1qZmskgj9YbzCOswQhmdTbLwW4Pc2CiF8xLmorc73aOmoeC4Z4h/lWXSQBcr0k9TdLFqT4GfFZiTLx//UaYO0Yf2JxrT1EJMVZy6vGRSHF/ADMUhLiOhLenGdBIX5H/jInyvwctkmOBRXxNzB3GEdzG2D+HKbpeyxYEL8Ae86bYT7nm8csWBhvgb9JN8X8NKdZz7ZVQzRgTgxcO5UdK4i7/QhNF/E9xmy32QC7xUIFFhTi6Wt0nSvJ/By2SY4FM3G3P2RfOFS3txLTzdyqpNXcwRJuFZYFQ9z17fQbDtHlA018pFxUtTKQuKOWWipuEOz6of4Jh+j3bxVVmxzkDrgD7oA74A64A+5AHw78AkOdplKAcgGEAAAAAElFTkSuQmCC"

# Trigger next track in music application
if [[ "$1" = 'cmus_next' ]]; then
  cmus-remote --next
  exit
fi
if [[ "$1" = 'itunes_next' ]]; then
  osascript -e 'tell application "iTunes" to next track'
  exit
fi
if [[ "$1" = 'music_next' ]]; then
  osascript -e 'tell application "Music" to next track'
  exit
fi
if [[ "$1" = 'spotify_next' ]]; then
  osascript -e 'tell application "Spotify" to next track'
  exit
fi
if [[ "$1" = 'pianobar_next' ]]; then
  echo -ne "\nn" > "$pianobar_ctlfile"
  exit
fi

# Ensure that pianobar fifo config file exists
if [ ! -e "$pianobar_ctlfile" ]; then
  mkfifo "$pianobar_ctlfile"
fi

BitBarDarkMode=${BitBarDarkMode}
current_source="$NONE"

# Get pid of music apps to see if they are currently running
cmus_pid=$(pgrep -x "$CMUS")
itunes_pid=$(pgrep -x "$ITUNES")
music_pid=$(pgrep -x "$MUSIC")
spotify_pid=$(pgrep -x "$SPOTIFY")
pianobar_pid=$(pgrep -x "$PIANOBAR")

# Keep track of music source
# Reorder items in for-loop to your liking to change order of precendece
# (i.e. if available, left-most audio source will be used first)
for s in "$CMUS" "$ITUNES" "$MUSIC" "$SPOTIFY" "$PIANOBAR"; do
  if [[ $s = "$CMUS" && $cmus_pid ]]; then
    current_source="$CMUS"
    break
  elif [[ $s = "$ITUNES" && $itunes_pid ]]; then
    current_source="$ITUNES"
    break
  elif [[ $s = "$MUSIC" && $music_pid ]]; then
    current_source="$MUSIC"
    break
  elif [[ $s = "$SPOTIFY" && $spotify_pid ]]; then
    current_source="$SPOTIFY"
    break
  elif [[ $s = "$PIANOBAR" && $pianobar_pid ]]; then
    current_source="$PIANOBAR"
    break
  fi
done

# Do not display menu icon if no audio source is active
if [[ $current_source = "$NONE" ]]; then
  exit
fi

# Set next track icon based on dark mode setup
if [[ "$BitBarDarkMode" ]]; then
  icon=$next_icon_dark
else
  icon=$next_icon_light
fi

# Trigger next track in correct music app
if [[ $current_source = "$CMUS" ]]; then
  echo " | image=$icon bash='$0' param1='cmus_next' terminal=false refresh=false"
elif [[ $current_source = "$ITUNES" ]]; then
  echo " | image=$icon bash='$0' param1='itunes_next' terminal=false refresh=false"
elif [[ $current_source = "$MUSIC" ]]; then
  echo " | image=$icon bash='$0' param1='music_next' terminal=false refresh=false"
elif [[ $current_source = "$SPOTIFY" ]]; then
  echo " | image=$icon bash='$0' param1='spotify_next' terminal=false refresh=false"
elif [[ $current_source = "$PIANOBAR" ]]; then
  echo " | image=$icon bash='$0' param1='pianobar_next' terminal=false refresh=false"
fi
