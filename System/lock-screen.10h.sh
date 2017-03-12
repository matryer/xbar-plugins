#!/bin/bash

# <bitbar.title>Screen Lock</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Chris Tomkins-Tinch</bitbar.author>
# <bitbar.author.github>tomkinsc</bitbar.author.github>
# <bitbar.desc>This plugin displays a menu with an item to lock the screen with one click (lock or login screen).</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/53064/12120421/e515718c-b39e-11e5-830b-bebe1c6445fc.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

if [ "$1" = 'lock' ]; then
  # To perform a sleep action
  # Requires "password after sleep or screen saver begins" to be set in Security preferences
  #osascript -e 'tell application "Finder" to sleep'

  # To perform a lock (login screen) action
  # Requires "Fast User Switching" to be enabled in system Login preferences
  /System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend
  exit
fi

echo "🔒"
echo '---'
echo "Lock Now | bash='$0' param1=lock terminal=false"
