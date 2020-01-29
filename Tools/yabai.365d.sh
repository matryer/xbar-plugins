#!/usr/bin/env bash
# <bitbar.title>Yabai status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Albert Groothedde</bitbar.author>
# <bitbar.author.github>alber70g</bitbar.author.github>
# <bitbar.desc>
# Shows the status of Yabai. Current space, and whether window is floating, sticky, on top and fullscreen.echo "setting up signals"
# ```
# yabai -m signal --add event=space_changed \
#   action="set SHELL=/bin/sh && open -g \"bitbar://refreshPlugin?name=yabai-window-info.*?.sh\""
# yabai -m signal --add event=window_resized \
#   action="set SHELL=/bin/sh && open -g \"bitbar://refreshPlugin?name=yabai-window-info.*?.sh\""
# yabai -m signal --add event=window_focused \
#   action="set SHELL=/bin/sh && open -g \"bitbar://refreshPlugin?name=yabai-window-info.*?.sh\""
# yabai -m signal --add event=application_activated \
#   action="set SHELL=/bin/sh && open -g \"bitbar://refreshPlugin?name=yabai-window-info.*?.sh\""
# echo "signals ready"
# ```
# </bitbar.desc>
# <bitbar.dependencies>yabai,jq</bitbar.dependencies>
PATH=$PATH:/usr/local/bin

windowFocused=$(yabai -m query --windows --space | jq '.[] | select(."focused"==1)')
space=$(yabai -m query --spaces --space | jq ".index")

if [[ $windowFocused ]]; then
  yabaiinfo=$(yabai -m query --windows --window |
    jq -r "[.floating,.sticky,.topmost,.\"zoom-fullscreen\"] | @sh")
  read -r -a props <<< "$yabaiinfo"

  echo ["$space"] ~"${props[0]}" s"${props[1]}" ^"${props[2]}" f"${props[3]}"
else
  # no window active
  echo ["$space"] w0
fi