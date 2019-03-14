#!/bin/bash

# Like the current track in iTunes or pianobar.
#
# Based on cmus script by mcchrish
# and Spotify script by alekseysotnikov
#
# metadata
# <bitbar.title>Music Controls - Like Track Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Likes the current track in iTunes or pianobar.</bitbar.desc>
# <bitbar.image>https://user-images.githubusercontent.com/1890600/54347955-56d68600-4648-11e9-8721-9dc087187be4.png</bitbar.image>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
ctlfile="$HOME/.config/pianobar/ctl"

# Trigger like track in music application
if [[ "$1" = 'itunes_like' ]]; then
  osascript -e 'tell application "iTunes"' -e 'if loved of current track is true then' -e 'set loved of current track to false' -e 'else' -e 'set loved of current track to true' -e 'end if' -e 'end tell'
  exit
fi
if [[ "$1" = 'pianobar_like' ]]; then
  echo -n + > $ctlfile
  exit
fi

# ensure that pianobar fifo config file exists
if [ ! -e "$ctlfile" ]; then
  mkfifo "$ctlfile"
fi

CMUS="cmus"
ITUNES="iTunes"
SPOTIFY="Spotify"
PIANOBAR="pianobar"

like_icon_light="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAgxJREFUaAXtV7lKBEEQXcUTxNDEQDATMdNExcRYBI9Ff0J/QgxENDZWEPwGExEjRcQr3kAwMRFkRfF6D6ZhZphpt3uqZl3pgsf2dHe9elVdc2ylEixUIFQgVOA/V6AdyRFqpkoO1dPAlJp6EHdokoN7IeI/VY6jRv8IZqIlbQKqvyOMa2WgeQ8sxkTHx7Hpvz2sQZ45AY5bysag1og3v6MaGWi1UFbLZM1p5CTCeZNxAlcizCWQDGWIN23ENVHTaKFVi8KqZc1rqZE3MZ/hfQ7sK5a9TO7csp5eesHERXrS9XoYDpeAaYOyfhmTsUWMJ7UBfALaCXxEsRrpDmx1s0lsfwC0kqiBmzFUrR/sB4B0EvvgdLnXCifJT+VngUTIYT67C4tyJRiEwxngexon8CVHU43vkk3ANQn6tBVVLvEi+4KIOw8ht/Bh0oVMIgEKWPJQ4ePjEeZ3l15seQdcW+gNPvQtZBInMA8FnR4quuAz5+GXcJFIYDnBmLzYwyWRZ01vI7ZAHUi3zxPm4tXlmHPpffQt3Ebg8DZ+HqdFHWNuIIORc1xL72/qP7WjmKBXjNcAm/G5vw7wBjaJHNocNNfi7XONQCMOwbj3HmASdYA3dOnGo+dLbBvweQp1w28HYBJ8kpVuu4g4IxB1FhxbAjzOFD3OHvkOklz5UcJKqECoQKhAqECoQKiAcAV+ACMY83xFU04+AAAAAElFTkSuQmCC"

like_icon_dark="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAjtJREFUaAXtlz1LA0EQhj3xE0SsbCwEOxE7bVRsrEWIH+if0D8hFiJaWysI/gYbEStFxK/aQrCxCYiiqPEZyMnm4l2ytzuJyA68ZHd255135+buci0twUIFQgVCBf5tBUqlUqtA84Cq5AifApOaB2jTJIe7UOY/Uc6jQ0/7PAp02JVZET4OYhvTSqd5D8wbos2x4f7DQ0p/H5dfxn9YarU0BI8a4uPhSPVOd49WC/3WMr/53E+gwUDJr+OyG7+XGrm8cyJ40BCdHA76TqjRQisZIpcy1nIt1XwTU0J5hvdYsC9n7F2B7yxjPbn0HEXRedJpNSfhELgAjTbJOWQlNm0zRG1gHXwCbfsggeSq2R1pelP9kE6AB6Bl8vKbSBXgY4EEvWBf4QR7cNrca27HIVkBFD0cRDjiv91uomyjSTwATh0OcSwctnm97keAfDZu5DiExESuYpxfZDynvxBxm0PIDbGlHHEVIc4HKLMtVLDWN8kTU8XsfAlpg25Yi6C9ij3b8c5yH1fhNXtb9qqPKzBHClvxoqoDzMrAxXwcYDFDwC5rgjTz0kZp5DX90j7gBSTtCcdPdWUMxJc0iZUWbI6RfCmpiPkR6E8qEl95jZ8Ka96XGjIODSmvjFeTws056xFYA28gtgNzT8PGZDfb54r5cL3JZS+4A2LSRnJDN9ZIOg++wBawfgoR0wm2gZg8yRprJN0B065Z4ZgBm6481vEk7bIOSgnwyZWSIrhDBUIFQgVCBUIFQgVUKvANOlADruetMRMAAAAASUVORK5CYII="

BitBarDarkMode=${BitBarDarkMode}
current_source=0

# Get pid of music apps to see if they are currently running
cmus_pid=$(pgrep -x $CMUS)
itunes_pid=$(pgrep -x $ITUNES)
spotify_pid=$(pgrep -x $SPOTIFY)
pianobar_pid=$(pgrep -x $PIANOBAR)

# Keep track of music source
# Reorder items in for-loop to your liking to change order of precendece
# (i.e. if available, left-most audio source will be used first)
for s in $CMUS $ITUNES $SPOTIFY $PIANOBAR; do
  if [[ $s = $CMUS && $cmus_pid -gt 0 ]]; then
    # cmus does not support likes
    exit
  elif [[ $s = $ITUNES && $itunes_pid -gt 0 ]]; then
    current_source=$ITUNES
    break
  elif [[ $s = $SPOTIFY && $spotify_pid -gt 0 ]]; then
    # spotify (applescript) api does not support likes
    exit
  elif [[ $s = $PIANOBAR && $pianobar_pid -gt 0 ]]; then
    current_source=$PIANOBAR
    break
  fi
done

# Like track icon based on dark mode setup and trigger like track in correct music app
if [[ "$BitBarDarkMode" ]]; then
  if [[ $current_source = $ITUNES ]]; then
    echo " | image=$like_icon_dark bash='$0' param1='itunes_like' terminal=false refresh=false"
  elif [[ $current_source = $PIANOBAR ]]; then
    echo " | image=$like_icon_dark bash='$0' param1='pianobar_like' terminal=false refresh=false"
  fi
else
  if [[ $current_source = $ITUNES ]]; then
    echo " | image=$like_icon_light bash='$0' param1='itunes_like' terminal=false refresh=false"
  elif [[ $current_source = $PIANOBAR ]]; then
    echo " | image=$like_icon_light bash='$0' param1='pianobar_like' terminal=false refresh=false"
  fi
fi
