#!/bin/bash	

#  <xbar.title>Swinsian</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Eddie Blundell</xbar.author>
#  <xbar.author.github>eddie</xbar.author.github>
#  <xbar.desc>View the current track name and artist from the Swinsian music player.</xbar.desc>
#  <xbar.dependencies>applescript</xbar.dependencies>

osascript <<'END'
  tell application "Swinsian"
		
      -- get the currently playing track
      set thetrack to current track
      
      -- get properties of the track
      set trackname to name of thetrack
      set trackartist to artist of thetrack
      set trackalbum to album of thetrack
      
      set info to "" & trackname & " by " & trackartist & " on " & trackalbum
    return info
  end tell
END
