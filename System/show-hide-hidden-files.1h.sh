#!/bin/bash

# <bitbar.title>Show/Hide Hidden Files</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Matthias Vogt</bitbar.author>
# <bitbar.author.github>matthias-vogt</bitbar.author.github>
# <bitbar.desc>Toggles showing/hiding hidden system files with one click</bitbar.desc>
# <bitbar.image>http://i.imgur.com/8qUKBTE.png</bitbar.image>

if [ "$1" = "YES" ] || [ "$1" = "NO" ]; then
	defaults write com.apple.finder AppleShowAllFiles $1 && killall Finder
fi

if [ $(defaults read com.apple.finder AppleShowAllFiles) == 'YES' ]; then
	echo "📂 | refresh=true terminal=false bash=$0 param1=NO"
else
	echo "📁 | refresh=true terminal=false bash=$0 param1=YES"
fi