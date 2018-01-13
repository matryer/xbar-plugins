#!/bin/bash

# <bitbar.title>chunkwm/skhd helper</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Shi Han NG</bitbar.author>
# <bitbar.author.github>shihanng</bitbar.author.github>
# <bitbar.desc>Plugin that displays desktop id and desktop mode of chunkwm.</bitbar.desc>
# <bitbar.dependencies>brew,chunkwm,skhd</bitbar.dependencies>

# Info about chunkwm, see: https://github.com/koekeishiya/chunkwm
# For skhd, see: https://github.com/koekeishiya/skhd

export PATH=/usr/local/bin:$PATH

if [[ "$1" = "stop" ]]; then
  brew services stop chunkwm
  brew services stop skhd
fi

if [[ "$1" = "restart" ]]; then
  brew services restart chunkwm
  brew services restart skhd
fi

echo "$(chunkc tiling::query --desktop id):$(chunkc tiling::query --desktop mode) | length=5"
echo "---"
echo "Restart chunkwm & skhd | bash='$0' param1=restart terminal=false"
echo "Stop chunkwm & skhd | bash='$0' param1=stop terminal=false"
