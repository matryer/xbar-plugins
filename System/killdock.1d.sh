#!/bin/bash
# <xbar.title>KillDock</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author.github>indapublic</xbar.author.github>
# <xbar.author>indapublic</xbar.author>
# <xbar.desc>Killall Dock</xbar.desc>

if [ "$1" = 'kill' ]; then
  killall Dock
  exit
fi

echo "⏏"
echo '---'
echo "Kill Dock | bash='$0' param1=kill terminal=false"
