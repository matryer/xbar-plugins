#!/bin/bash

# <xbar.title>yabai/skhd helper</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>James Won</xbar.author>
# <xbar.author.github>jwon</xbar.author.github>
# <xbar.desc>Plugin that displays desktop id and desktop mode of yabai.</xbar.desc>
# <xbar.dependencies>brew,yabai,skhd</xbar.dependencies>

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
