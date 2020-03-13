#!/bin/bash

# Like the current track in iTunes, Music or pianobar.
#
# Special thanks to Google for providing the open-source icons: https://github.com/google/material-design-icons
# and to mcchrish and alekseysotnikov for their helpful existing BitBar scripts
#
# metadata
# <bitbar.title>Music Controls - Like Track Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Likes the current track in iTunes, Music or pianobar.</bitbar.desc>
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

like_icon_light="iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAcFJREFUWAntV00rRUEYvr4iZYFQPlY2smYntnaShYWdkj1LUn6BIksLZcVPkI2thb2kG1vcLEQhnqfOq9vcOeO8M3Nup5y3nmbO+/G8z8yZzp1bqZT2T3dgGusmCmPHUEIUwvqg4j1Bf6ii1lAC1K8DnQnWIvAFUXBB98B3As5jLBI0fraIMhEj44IfVZyqC4ug8zjUepZxixjZJcaabofoKALMcb/ZarrR8NUhiDHmqK3FqJjF8wjAb0s9eo1nfm/aAZd9IFgDnutGzgUSe4DvErDaFLxPgPkK8npmL/Z0Gg9kFchLhPCyR+bDP4Dk6xxFkZs9VMZDyW+KrCjWSE6vA0/1bcAJEEvMUcKJIcx2UR4qaidMQmP1KlxfHsI+UbPSSBfH8+ghiDW52CRYfV/bRFZFmrvLTFZSS17m2sIJsiwm1XWHiO8ru01l9QwMO8RUEZtPwHma6OA/AOD+tWXMzEa8YmwDvOCLcb4FvAFm/pIkxRgPjAZneB51EI8hdmrU7Dny1SH5ob1B5Zyimrms4W5dKeqcqT2I1oAN4K9LmY2oA85N4AXosiVofUMoGNQWWfLJob5yWHhKV7kDzh34ASdLIkzm2nPzAAAAAElFTkSuQmCC"
like_icon_dark="iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAYAAADhAJiYAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAeVJREFUWAntlj9LA0EQxXNRUQIWJkbBP1UasY6daGsnYmFhJ4ilEEtF8BNYWFsIVvoRgo2thb2VaGsMFqKgEn+jWTj3cre7d5cQ8AYeu7cz8+bdZHO7uVxm/7EDrVZrQdA3746YM0FfCEJIEby3UUoqKp+UgPwdMNzGdgp88SnoSh48AGUyT+Ml44mi+JpS4htX47GlkIWIK58QNa2nQO1OQfWKUtBhrLgz/mYk+b1rEUV3I3zpu+hIAbx26IxaEl8hTmXPnwTJEs/ToKhhTHuW780giLIPnE3w7BtlrqB8j57nXbMeNARVQQP0yqRWNajEt0KAbNb7HiiSGnabn8AyuO2iKOEu+/pgnpIgG7feBVHCGWvD50gcAOcpijoVTnM7DBGQHKUg6tBQxs2NoC3wFUPYJzmbttX+fIdMSRA/EeN652nwnRk3cSu/9dGBmPkYYqROidw5VdA0WguCaNFEFuG3zs0ERXTR3sUemAJhJkfAShtRR47rnyFcIMU2OqiRK8YBkAv+j8kc7IM3oNu6iks8wnyisV/yPBNGjG8WXGg5x2HxzusQq4P2jvmyLYHEAskRu7HNi4yDaBQ0QQ2YLmUBLnKGwB54ASOBANcFSCbBhGueHi8cwO3KoZNkz1kHLDrwDQpoMjh/irN5AAAAAElFTkSuQmCC"

# Trigger like track in music application
if [[ "$1" = 'itunes_like' ]]; then
  osascript -e 'tell application "iTunes"' -e 'if loved of current track is true then' -e 'set loved of current track to false' -e 'else' -e 'set loved of current track to true' -e 'end if' -e 'end tell'
  exit
fi
if [[ "$1" = 'music_like' ]]; then
  osascript -e 'tell application "Music"' -e 'if loved of current track is true then' -e 'set loved of current track to false' -e 'else' -e 'set loved of current track to true' -e 'end if' -e 'end tell'
  exit
fi
if [[ "$1" = 'pianobar_like' ]]; then
  echo -ne "\n+" > "$pianobar_ctlfile"
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
    # cmus does not support likes
    exit
  elif [[ $s = "$ITUNES" && $itunes_pid ]]; then
    current_source="$ITUNES"
    break
  elif [[ $s = "$MUSIC" && $music_pid ]]; then
    current_source="$MUSIC"
    break
  elif [[ $s = "$SPOTIFY" && $spotify_pid ]]; then
    # spotify (applescript) api does not support likes
    exit
  elif [[ $s = "$PIANOBAR" && $pianobar_pid ]]; then
    current_source="$PIANOBAR"
    break
  fi
done

# Do not display menu icon if no audio source is active
if [[ $current_source = "$NONE" ]]; then
  exit
fi

# Set like track icon based on dark mode setup
if [[ "$BitBarDarkMode" ]]; then
  icon=$like_icon_dark
else
  icon=$like_icon_light
fi

# Trigger like track in correct music app
if [[ $current_source = "$ITUNES" ]]; then
  echo " | image=$icon bash='$0' param1='itunes_like' terminal=false refresh=false"
elif [[ $current_source = "$MUSIC" ]]; then
  echo " | image=$icon bash='$0' param1='music_like' terminal=false refresh=false"
elif [[ $current_source = "$PIANOBAR" ]]; then
  echo " | image=$icon bash='$0' param1='pianobar_like' terminal=false refresh=false"
fi
