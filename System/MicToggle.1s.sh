#!/bin/bash

# <xbar.title>Mic Toggle</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alejandro Narvaja</xbar.author>
# <xbar.author.github>alebarbaja</xbar.author.github>
# <xbar.desc>Toggle microphone on/off and display its status.</xbar.desc>
# <xbar.image>https://i.postimg.cc/nzShGcfN/mute-unmute-mic.gif</xbar.image>
# <xbar.dependencies>osascript</xbar.dependencies>

# Get the current input volume
current_volume=$(osascript -e "input volume of (get volume settings)")

# If the volume is 0, the mic is muted
if [ "$current_volume" == "0" ]; then
  echo "🔇"
  echo "---"
  echo "Activate Mic | bash='$0' param1=unmute terminal=false"
else
  echo "🔊"
  echo "---"
  echo "Mute Mic | bash='$0' param1=mute terminal=false"
fi

# Handle user click
if [ "$1" == "mute" ]; then
  osascript -e "set volume input volume 0"
elif [ "$1" == "unmute" ]; then
  osascript -e "set volume input volume 100"
fi
