#!/bin/bash

# Plays / pauses the current track in cmus, iTunes, Spotify or pianobar.
#
# Special thanks to Google for providing the open-source icons: https://github.com/google/material-design-icons
# and to mcchrish and alekseysotnikov for their helpful existing BitBar scripts
#
# metadata
# <bitbar.title>Music Controls - Play / Pause Button</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastián Barschkis</bitbar.author>
# <bitbar.author.github>sebbas</bitbar.author.github>
# <bitbar.desc>Plays / pauses the current track in cmus, iTunes, Spotify or pianobar.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/sebbas/music-controls-bitbar/master/music-controls-screenshot.png</bitbar.image>
# <bitbar.abouturl>http://github.com/sebbas/music-controls-bitbar</bitbar.abouturl>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
export LC_CTYPE="UTF-8"

pianobar_ctlfile="$HOME/.config/pianobar/ctl"

NONE="none"
CMUS="cmus"
ITUNES="iTunes"
SPOTIFY="Spotify"
PIANOBAR="pianobar"

playpause_icon_light="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAABYlAAAWJQFJUiTwAAAA5klEQVRo3u3YIU5DQRSF4Q9SGhSmCQaNrMCygmp2wQ5gB6yANWDQXQEWgaxGkZqaiiZkUGOeQLzhdbjJ/ZOj5/wzk7y5jyRJkiRJkqSFMsiQF1z0LtkiUPCJ295FWwQKvvGEWe/CYwVq3nHdu3SLQMEe972LtwjUrLGILFDwhVVkgZpnnEcWKNhgGVmg4IAHnEYVqHnDVWSBgh3uflv8aMfUwHzqBfIKDXLAoz+8HSdHFNjgZtLtnlAg7IdsK/BTYo3LnuXHCoR+Tn8IOtDUkfKsd+ExAqGH+lf//LdKkiRJkiRJVH4AdugWuakjkxMAAAAASUVORK5CYII="
playpause_icon_dark="iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAYAAABXAvmHAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAABYlAAAWJQFJUiTwAAAA8UlEQVRo3u3YMUoDQRSA4SgqgqCFYGNtaZE2J7D2FrmB3sATeAYb65zA1sIytZWkSWMRkM9qCIhKyM7u5MH72sC++ZdNZiejUUoppZRS2poffvn8CacV550NHQDvmFSad9IiAL7wgIOO8/ZbBRSvuKq6iIED4BPT1mvvElDMcB45AD5wEzmgeMTxBvP2djUA5riOHAAr3Pnj5zJCQPGCy8gBsMTtf/Pr7mr9OOr16kM/QhECVrgX9Es8x7j3u95TwEYb2S4GLAR+lZjhotniOwSEfp1+E/RAU46Uh63XvE1AtUN9i4BnFf9WSSmllFJKa98ifb5bWzSaAAAAAABJRU5ErkJggg=="

# Play / pause music application
if [[ "$1" = 'cmus_playpause' ]]; then
  cmus-remote --pause
  exit
fi
if [[ "$1" = 'itunes_playpause' ]]; then
  osascript -e 'tell application "iTunes" to playpause'
  exit
fi
if [[ "$1" = 'spotify_playpause' ]]; then
  osascript -e 'tell application "Spotify" to playpause'
  exit
fi
if [[ "$1" = 'pianobar_playpause' ]]; then
  echo -ne "\np" > "$pianobar_ctlfile"
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
    current_source="$PIANOBAR"
    break
  fi
done

# Do not display menu icon if no audio source is active
if [[ $current_source = "$NONE" ]]; then
  exit
fi

# Set play / pause icon based on dark mode setup
if [[ "$BitBarDarkMode" ]]; then
  icon=$playpause_icon_dark
else
  icon=$playpause_icon_light
fi

# Play / pause track in correct music app
if [[ $current_source = "$CMUS" ]]; then
  echo " | image=$icon bash='$0' param1='cmus_playpause' terminal=false refresh=false"
elif [[ $current_source = "$ITUNES" ]]; then
  echo " | image=$icon bash='$0' param1='itunes_playpause' terminal=false refresh=false"
elif [[ $current_source = "$SPOTIFY" ]]; then
  echo " | image=$icon bash='$0' param1='spotify_playpause' terminal=false refresh=false"
elif [[ $current_source = "$PIANOBAR" ]]; then
  echo " | image=$icon bash='$0' param1='pianobar_playpause' terminal=false refresh=false"
fi
