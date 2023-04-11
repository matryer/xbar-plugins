#!/usr/bin/env bash

# <xbar.title>Zoom Mute Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Dustin</xbar.author>
# <xbar.author.github>dustincredible</xbar.author.github>
# <xbar.desc>Reports the mute status of a Zoom meeting. Relies on menu bar item names, so this will only work if your Zoom app is using english. Only partially works for Zoom webinars.</xbar.desc>
# <xbar.image>https://dustincredible.github.io/images/zoom-mute-xbar.jpg</xbar.image>
# <xbar.abouturl>https://dustin.lol/post/2021/better-zoom-mute/</xbar.abouturl>

# Check the Enable Global Shortcut for Mute/unmute in your Zoom app settings
if [[ "$1" = "toggle" ]]; then
osascript -e 'tell application "System Events" to keystroke "a" using {command down, shift down}'
fi

if [[ "$1" = "launch" ]]; then
osascript -e '
	tell application "zoom.us" 
		activate
	end tell
	'
fi

zm_status=$(osascript -e'
	tell application "System Events"
		if (get name of every application process) contains "zoom.us" then
			tell application "System Events" to tell application process "zoom.us"
				if menu item "Join Audio" of menu 1 of menu bar item "Meeting" of menu bar 1 exists then
					return 1
				else
					if (menu item "Mute audio" of menu 1 of menu bar item "Meeting" of menu bar 1 exists) or (menu item "Mute telephone" of menu 1 of menu bar item "Meeting" of menu bar 1 exists) then
						return true
					else
						if (menu item "Unmute audio" of menu 1 of menu bar item "Meeting" of menu bar 1 exists) or (menu item "Unmute telephone" of menu 1 of menu bar item "Meeting" of menu bar 1 exists) then
							return false
						else
							return off
						end if
					end if
				end if
			end tell
		else
			return off
		end if
	end tell
	');


if [ "$zm_status" = "true" ]; then
  echo "🟢" #emoji allowed for example: echo "🗣"
  echo ---
  echo "Mute| bash='$0' param1=toggle terminal=false"
  exit
elif [ "$zm_status" = "false" ]; then
  echo "🔴" #emoji allowed for example: echo "🤫"
  echo ---
  echo "Unmute| bash='$0' param1=toggle terminal=false"
  exit
elif [ "$zm_status" = "off" ]; then
  echo "⚪️"
  echo ---
  echo "Launch Zoom| bash='$0' param1=launch terminal=false"
  exit
elif [ "$zm_status" = "1" ]; then
  echo "🤐"
  echo ---
  echo "Join Audio| bash='$0' param1=toggle terminal=false"
  exit
fi