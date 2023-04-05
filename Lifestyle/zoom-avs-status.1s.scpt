#!/usr/bin/osascript

# <bitbar.title>Zoom Mic/Video/Share Status for Zoom</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>romansavrulin</bitbar.author>
# <bitbar.author.github>romansavrulin</bitbar.author.github>
# <bitbar.desc>Zoom Audio/Video/Share Status</bitbar.desc>
# <bitbar.dependencies>Applescript</bitbar.dependencies>

# <swiftbar.hideAbout>true</swiftbar.hideAbout>
# <swiftbar.hideRunInTerminal>true</swiftbar.hideRunInTerminal>
# <swiftbar.hideLastUpdated>true</swiftbar.hideLastUpdated>
# <swiftbar.hideDisablePlugin>true</swiftbar.hideDisablePlugin>
# <swiftbar.hideSwiftBar>true</swiftbar.hideSwiftBar>

property appName : "zoom.us"

set zoomPath to "/Applications/" & appName & ".app" as POSIX file as alias

set conferenceMenuTitle to localized string "Meeting" in bundle zoomPath
set muteBtnTitle to localized string "Unmute Audio" in bundle zoomPath
set videoBtnTitle to localized string "Start Video" in bundle zoomPath
set shareBtnTitle to localized string "Start Share" in bundle zoomPath

property status : ""
property muteState : ":mic.slash:"
property videoState : ":video.slash:"
property shareState : ":rectangle.stack.badge.person.crop:"
property barItem : ":questionmark.video:|sfcolor=#888888"

if application appName is running then
	tell application "System Events"
		tell application process appName
			if exists (menu bar item conferenceMenuTitle of menu bar 1) then
				set status to "conf"
				if not exists (menu item muteBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set muteState to ":mic.fill:"
					set status to "onAir"
				end if
				
				if not exists (menu item videoBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set videoState to ":video.fill:"
					set status to "onAir"
				end if
				
				if not exists (menu item shareBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set shareState to ":rectangle.stack.fill.badge.person.crop:"
					set status to "onAir"
				end if			   
			end if
		end tell
	end tell
end if

if status is not ""
	set barItem to shareState & videoState & muteState
	if status = "onAir"
		set barItem to barItem & "|sfcolor=#ff4d40"
	else if status = "conf"
	    set barItem to barItem & "|sfcolor=#00ff00"
	end if	
end if


return barItem & "
---
Zoom Audio/Video/Share Status"
