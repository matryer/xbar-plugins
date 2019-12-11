#!/bin/bash

# <bitbar.title>yabai/skhd helper</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>James Won</bitbar.author>
# <bitbar.author.github>jwon</bitbar.author.github>
# <bitbar.desc>Plugin that displays desktop id and desktop mode of yabai.</bitbar.desc>
# <bitbar.dependencies>brew,yabai,skhd</bitbar.dependencies>

# Info about yabai, see: https://github.com/koekeishiya/yabai
# For skhd, see: https://github.com/koekeishiya/skhd
# This plugin has been forked from https://github.com/matryer/bitbar-plugins/blob/master/Tools/chunkwm_skhd.1s.sh

export PATH=/usr/local/bin:$PATH

if [[ "$1" = "stop" ]]; then
  brew services stop yabai
  brew services stop skhd
fi

if [[ "$1" = "restart" ]]; then
  brew services restart yabai
  brew services restart skhd
fi

echo "$(yabai -m query --spaces --display | jq 'map(select(."focused" == 1))[-1].index'):$(yabai -m query --spaces --display | jq -r 'map(select(."focused" == 1))[-1].type') | length=5"
echo "---"
echo "Restart yabai & skhd | bash='$0' param1=restart terminal=false"
echo "Stop yabai & skhd | bash='$0' param1=stop terminal=false"
