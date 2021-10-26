#!/bin/bash

# <xbar.title>chunkwm/skhd helper</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Shi Han NG</xbar.author>
# <xbar.author.github>shihanng</xbar.author.github>
# <xbar.desc>Plugin that displays desktop id and desktop mode of chunkwm.</xbar.desc>
# <xbar.dependencies>brew,chunkwm,skhd</xbar.dependencies>

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
