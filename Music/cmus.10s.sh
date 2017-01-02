#!/bin/bash

# Displays the current track being played by `cmus`, a console music player.
# All thanks to the cmus-remote.
#
# based on Spotify script by Jason Tokoph (jason@tokoph.net)
#
# Choose to launch cmus in iTerm2 (version 2.9.20150414+ only) or Terminal
#
# Metadata:
# <bitbar.title>Cmus Now Playing</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Michael Chris Lopez</bitbar.author>
# <bitbar.author.github>mcchrish</bitbar.author.github>
# <bitbar.desc>Displays currently playing song from cmus. Control cmus in menubar.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/qeZCB0a.png</bitbar.image>

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"


if [ "$1" = 'launch-iterm' ]; then
  if [ "$(osascript -e 'application "iTerm" is running')" = "false" ]; then
    osascript -e 'tell application "iTerm" to activate'
    osascript -e 'tell application "iTerm" to tell current session of current window to write text "cmus"'
  else
    # Then create new tab
    osascript -e 'tell application "iTerm" to tell current window to set newTab to (create tab with default profile)'
    osascript -e 'tell application "iTerm" to tell current window to tell current tab to tell current session to write text "cmus"'
  fi

  exit
fi

if [ "$1" = 'launch-terminal' ]; then
  if [ "$(osascript -e 'application "Terminal" is running')" = "false" ]; then
    osascript -e 'tell application "Terminal" to activate'
    osascript -e 'tell application "Terminal" to do script "cmus" in window 1'
  else
    # Then create a new tab
    osascript -e 'tell application "System Events" to keystroke "t" using command down'
    osascript -e 'tell application "Terminal" to do script "cmus" in tab 2 of window 1'
  fi

  exit
fi

if cmus-remote -C status > /dev/null 2>&1; then
  echo "♫"
  echo "---"
  echo "cmus is not running"
  echo "Launch cmus in iTerm | bash='$0' param1=launch-iterm terminal=false refresh=true"
  echo "Launch cmus in Terminal | bash='$0' param1=launch-terminal terminal=false refresh=true"
  exit
fi

state=$(cmus-remote -C status | sed -n 1p | cut -d " " -f2)

if [ "$1" = 'playpause' ]; then
  cmus-remote --pause
  exit
fi

if [ "$1" = 'previous' ]; then
  cmus-remote --prev
  exit
fi

if [ "$1" = 'next' ]; then
  cmus-remote --next
  exit
fi

if [ "$state" = "playing" ]; then
  state_icon="▶"
else
  state_icon="❚❚"
fi

track=$(cmus-remote -C "format_print %{title}")
artist=$(cmus-remote -C "format_print %{artist}")
album=$(cmus-remote -C "format_print %{album}")

echo "$state_icon $track - $artist | length=40"
echo "---"

case "$0" in
  *\ * )
   echo "Your script path | color=#ff0000"
   echo "($0) | color=#ff0000"
   echo "has a space in it, which BitBar does not support. | color=#ff0000"
   echo "Play/Pause/Next/Previous buttons will not work. | color=#ff0000"
  ;;
esac

echo "Track: $track | color=#333333 length=40"
echo "Artist: $artist | color=#333333 length=40"
echo "Album: $album | color=#333333 length=40"

echo "---"

if [ "$state" = "playing" ]; then
  echo "Pause | bash='$0' param1=playpause terminal=false refresh=true"
  echo "Previous | bash='$0' param1=previous terminal=false refresh=true"
  echo "Next | bash='$0' param1=next terminal=false refresh=true"
else
  echo "Play | bash='$0' param1=playpause terminal=false refresh=true"
fi
