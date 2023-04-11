#!/usr/bin/osascript

#  <xbar.title>Zoom Mic/Video/Share Status for Zoom</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Roman Savrulin</xbar.author>
#  <xbar.author.github>romansavrulin</xbar.author.github>
#  <xbar.desc>Zoom Audio/Video/Share Status</xbar.desc>
#  <xbar.image>https://user-images.githubusercontent.com/1313542/230366563-9deb4724-8419-4585-8593-a2bdfeacc1c2.png</xbar.image>
#  <xbar.dependencies>Applescript</xbar.dependencies>
#  <xbar.abouturl>https://github.com/matryer/xbar-plugins/pull/1937</xbar.abouturl>
#  <xbar.var>string(APP_NAME="zoom.us"): Zoom app path inside /Application folder</xbar.var>

set appName to system attribute "APP_NAME"

set zoomPath to "/Applications/" & appName & ".app" as POSIX file as alias

set conferenceMenuTitle to localized string "Meeting" in bundle zoomPath
set muteBtnTitle to localized string "Unmute Audio" in bundle zoomPath
set videoBtnTitle to localized string "Start Video" in bundle zoomPath
set shareBtnTitle to localized string "Start Share" in bundle zoomPath

property status : ""
property muteState : ":mute:"
property videoState : ""
property shareState : ""
property barItem : ":free:"

if application appName is running then
	tell application "System Events"
		tell application process appName
			if exists (menu bar item conferenceMenuTitle of menu bar 1) then
				set status to "conf"

				if not exists (menu item muteBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set muteState to ":sound:"
					set status to "onAir"
				end if
				
				if not exists (menu item videoBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set videoState to ":camera:"
					set status to "onAir"
				end if
				
				if not exists (menu item shareBtnTitle of menu 1 of menu bar item conferenceMenuTitle of menu bar 1) then
					set shareState to ":computer:"
					set status to "onAir"
				end if			   
			end if
		end tell
	end tell
end if

if status is not ""
	set barItem to shareState & videoState & muteState
	if status = "onAir"
		set barItem to barItem & ""
	else if status = "conf"
	    set barItem to barItem & ""
	end if	
end if


return barItem & "
---
Zoom Audio/Video/Share Status"
