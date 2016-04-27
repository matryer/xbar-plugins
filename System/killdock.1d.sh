#!/bin/bash
# <bitbar.title>KillDock</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>indapublic</bitbar.author>
# <bitbar.author.github>indapublic</bitbar.author.github>
# <bitbar.desc>Kill Dock. It useful when Dock is missing on main display</bitbar.desc>

if [ "$1" = 'kill' ]; then
  killall Dock
  exit
fi

echo "⏏"
echo '---'
echo "Kill Dock | bash=$0 param1=kill terminal=false"
