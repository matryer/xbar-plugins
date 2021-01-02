#!/bin/bash

# <bitbar.title>iTerm SSH Dropdown Menu</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Gerd Naschenweng</bitbar.author>
# <bitbar.author.github>magicdude4eva</bitbar.author.github>
# <bitbar.desc>Provides a simple menu of ssh hosts when clicked on opens a new ssh session in terminal</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

SSHFILE="$HOME/.ssh/config"

echo "💀 ssh | color=#ff0000"
echo "---"
#Enter list of servers below give it a name and an address
#After adding or changing refresh plugins
echo "Some Server | bash='$0' param1=ssh param2=root@127.0.0.1 terminal=false"

echo "---"
echo "All Servers"
awk  '/^Host / && !/(\*)/ {print "--" $2" | bash='$0' param1=ssh param2="$2" terminal=false"}' $SSHFILE

echo "Amazon Servers"
awk  '/^Host live_.*_amazon.com$/ && !/(\*)/ {print "--" $2" | bash='$0' param1=ssh param2="$2" terminal=false"}' $SSHFILE

echo "---"
echo "Synology | bash='$0' param1=ssh param2=root@192.168.1.97 terminal=false"

if [ "$1" = 'ssh' ]; then
  if [ "$(osascript -e 'application "iTerm2" is running')" = "false" ]; then
    osascript -e 'tell application "iTerm"' \
      -e 'tell current session of current tab of current window to write text "ssh '$2'"' \
      -e 'tell application "iTerm2" to activate' -e 'end tell'
  else
    osascript -e 'tell application "iTerm2"' \
      -e 'tell current window to set newTab to (create tab with default profile)' \
      -e 'tell current session of current tab of current window to write text "ssh '$2'"' \
      -e 'tell application "iTerm2" to activate' -e 'end tell'
  fi

  exit
fi
