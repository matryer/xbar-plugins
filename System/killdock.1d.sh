#!/bin/bash
# <bitbar.title>KillDock</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author.github>indapublic</bitbar.author.github>
# <bitbar.author>indapublic</bitbar.author>
# <bitbar.desc>Killall Dock</bitbar.desc>

if [ "$1" = 'kill' ]; then
  killall Dock
  exit
fi

echo "⏏"
echo '---'
echo "Kill Dock | bash='$0' param1=kill terminal=false"
