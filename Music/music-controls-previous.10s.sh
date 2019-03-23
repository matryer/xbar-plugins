#!/bin/bash

# Plays the previous track in cmus, iTunes or Spotify.
#
# Special thanks to Google for providing the open-source icons: https://github.com/google/material-design-icons
# and to mcchrish and alekseysotnikov for their helpful existing BitBar scripts
#
# metadata
# <bitbar.title>Music Controls - Previous Track Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Plays the previous track in cmus, iTunes or Spotify.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/sebbas/music-controls-bitbar/master/music-controls-screenshot.png</bitbar.image>
# <bitbar.abouturl>http://github.com/sebbas/music-controls-bitbar</bitbar.abouturl>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
export LC_CTYPE="UTF-8"

NONE="none"
CMUS="cmus"
ITUNES="iTunes"
SPOTIFY="Spotify"
PIANOBAR="pianobar"

prev_icon_light="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAYBJREFUaAXtWDFKBEEQPASFS0yES4wNLzD1Bcb+whfcD/QHvsHk4nuBqYGhsZGYGB0oojVgQ9Ess9t7PcFADTQzPdNd1V17eju3WGhIASkgBaSAFJACUkAKRBT4RTBbJLcWe4rDx1pA1hkXX9YZ4wogb7AsvGpNmQ0cg+kO9vNffFcNXKDoZyrchMFW22FENs9hu0XSHmYYPM/BC+UwWVlHxhmCdzCPwX4Eb1Ysk0UauAbb+0jxEbxZxZekaANL5DwM5Hkc8wtH02FENtfI1jh8hVnslLmGl3LmixgCPcLmBvYF8/Fj/hBe6p4vwIOfY+MJ5uOm+h4v3feFMMENnE+Yj4n4jDe6Lo86c5xkgrXC8mp6nq4/QtaM/RF/Y8M3POYbRrPZF1AjusRhl/9Guamuv8i4kfIq8QHzT9D7nNNkfQjhChV1+zLHanb7Os1NlAvNC+yQJ8p4k9eZhOVKeQ/r8krJinV7qecmys8qW97QWgpIASkgBaSAFJACUqCuwB/4tRa49HkbAgAAAABJRU5ErkJggg=="
prev_icon_dark="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAAVlpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IlhNUCBDb3JlIDUuNC4wIj4KICAgPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4KICAgICAgPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIKICAgICAgICAgICAgeG1sbnM6dGlmZj0iaHR0cDovL25zLmFkb2JlLmNvbS90aWZmLzEuMC8iPgogICAgICAgICA8dGlmZjpPcmllbnRhdGlvbj4xPC90aWZmOk9yaWVudGF0aW9uPgogICAgICA8L3JkZjpEZXNjcmlwdGlvbj4KICAgPC9yZGY6UkRGPgo8L3g6eG1wbWV0YT4KTMInWQAAAZhJREFUaAXtmDtKhEEQhH8EBRMTwcTY0MDUExh7C0/gDfQGnsHE2BOYGhgaG4mJkaDI+k3Q0FvszuN/IAM9UMz0TnX1dLGPf3YYYoQD4UA4EA6EA+FAOBAOVDuwklGdWCAiewDuC7Tp23L+1XTFYUDzHLwl7Tn0shqpiB9ZcmETnV1wA35Ns5AyfdsK2TxWkfwT8Gw6No/Vq86zQjZXJzoiuVfgyzT87GjLLH2xtG6pAv0QPKa8baNFbxRXC9eKkHcB3jVf41q90bzWgvD3wZ3mbYtHH6w2UQvn8uCeglfNycU5vVn2tPgmUTg74Bp8K78Ub9Kb9TU9gIqzfwyelFcbq97ssR7EF2DvEnwqpyX2ejXrnRpSA2evgfs/VHVTT8F+v28ha4Ym7EP8ow2XYtNYbNYD5ArBPQP9fY36pmig3x8yaSQ9SnyA7PA5i6y1eksRco9Anw9zvlGa6PNxWppIF5oXsDY8Z5H1WjWCKUVIT1fKW9DfldI3TgN9XuqlifS3yoN/LdbhQDgQDoQD4UA4EA6EA3kH/gDrwKZSdCEqDwAAAABJRU5ErkJggg=="

# Trigger previous track in music application
if [[ "$1" = 'cmus_prev' ]]; then
  cmus-remote --prev
  exit
fi
if [[ "$1" = 'itunes_prev' ]]; then
  osascript -e 'tell application "iTunes" to previous track'
  exit
fi
if [[ "$1" = 'spotify_prev' ]]; then
  osascript -e 'tell application "Spotify" to previous track'
  exit
fi

BitBarDarkMode=${BitBarDarkMode}
current_source="$NONE"

# Get pid of music apps to see if they are currently running
cmus_pid=$(pgrep -x "$CMUS")
itunes_pid=$(pgrep -x "$ITUNES")
spotify_pid=$(pgrep -x "$SPOTIFY")
pianobar_pid=$(pgrep -x "$PIANOBAR")

# Keep track of music source
# Reorder items in for -loop to your liking to change order of precendece
# (i.e. if available, left-most audio source will be used first)
for s in "$CMUS" "$ITUNES" "$SPOTIFY" "$PIANOBAR"; do
  if [[ $s = "$CMUS" && $cmus_pid ]]; then
    current_source="$CMUS"
    break
  elif [[ $s = "$ITUNES" && $itunes_pid ]]; then
    current_source="$ITUNES"
    break
  elif [[ $s = "$SPOTIFY" && $spotify_pid ]]; then
    current_source="$SPOTIFY"
    break
  elif [[ $s = "$PIANOBAR" && $pianobar_pid ]]; then
    # pianobar does not support previous song trigger
    exit
  fi
done

# Do not display menu icon if no audio source is active
if [[ $current_source = "$NONE" ]]; then
  exit
fi

# Set previous track icon based on dark mode setup
if [[ "$BitBarDarkMode" ]]; then
  icon=$prev_icon_dark
else
  icon=$prev_icon_light
fi

# Trigger previous track in correct music app
if [[ $current_source = "$CMUS" ]]; then
  echo " | image=$icon bash='$0' param1='cmus_prev' terminal=false refresh=false"
elif [[ $current_source = "$ITUNES" ]]; then
  echo " | image=$icon bash='$0' param1='itunes_prev' terminal=false refresh=false"
elif [[ $current_source = "$SPOTIFY" ]]; then
  echo " | image=$icon bash='$0' param1='spotify_prev' terminal=false refresh=false"
fi
