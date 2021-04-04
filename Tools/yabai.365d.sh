#!/usr/bin/env bash
# <xbar.title>Yabai status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Albert Groothedde</xbar.author>
# <xbar.author.github>alber70g</xbar.author.github>
# <xbar.desc>
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
# </xbar.desc>
# <xbar.dependencies>yabai,jq</xbar.dependencies>
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