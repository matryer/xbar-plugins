#!/usr/bin/env bash
# <bitbar.title>Yabai status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Albert Groothedde</bitbar.author>
# <bitbar.author.github>alber70g</bitbar.author.github>
# <bitbar.desc>Shows the status of Yabai. Current space, and whether window is floating, sticky, on top and fullscreen.</bitbar.desc>
# <bitbar.dependencies>yabai,jq</bitbar.dependencies>
PATH=$PATH:/usr/local/bin

windowFocused=$(yabai -m query --windows --space | jq '.[] | select(."focused"==1)')
space=$(yabai -m query --spaces --space | jq ".index")

if [[ $windowFocused ]]; then
  yabaiinfo=$(yabai -m query --windows --window |
    jq -r "[.floating,.sticky,.topmost,.\"zoom-fullscreen\"] | @sh")
  props=($yabaiinfo)

  echo [$space] ~${props[0]} s${props[1]} ^${props[2]} f${props[3]}
else
  # no window active
  echo [$space] w0
fi
