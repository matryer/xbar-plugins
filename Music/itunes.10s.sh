#!/bin/bash

# Get current iTunes status with play/pause button
#
# based on Spotify script by Jason Tokoph (jason@tokoph.net),
# tweaked by Dan Turkel (daturkel@gmail.com),
# additionally tweaked by Aleš Farčnik (@alesf)
#
# Shows current track information from iTunes
# 10 second refresh might be a little too quick. Tweak to your liking.

# metadata
# <bitbar.title>iTunes Now Playing</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Dan Turkel, Jason Tokoph, Aleš Farčnik</bitbar.author>
# <bitbar.author.github>daturkel</bitbar.author.github>
# <bitbar.desc>Display currently playing iTunes song with artwork. Play/pause, skip forward, skip backward.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/lBfoFdY.png</bitbar.image>

if [ "$1" = 'launch' ]; then
  osascript -e 'tell application "iTunes" to activate'
  exit
fi

if [ "$1" = 'open' ]; then
  osascript -e 'tell application "iTunes" to reopen'
  osascript -e 'tell application "iTunes" to activate'
  exit
fi

if [ "$(osascript -e 'application "iTunes" is running')" = "false" ]; then
  echo "♫ | size=12"
  echo "---"
  echo "iTunes is not running"
  echo "Launch iTunes | bash='$0' param1=launch terminal=false"
  exit
fi

if [ "$1" = 'playpause' ]; then
  osascript -e 'tell application "iTunes" to playpause'
  exit
fi

if [ "$1" = 'previous' ]; then
  osascript -e 'tell application "iTunes" to previous track'
  exit
fi

if [ "$1" = 'next' ]; then
  osascript -e 'tell application "iTunes" to next track';
  exit
fi

BitBarDarkMode=${BitBarDarkMode}
if [ "$BitBarDarkMode" ]; then
  COLOR0="#666666"
  COLOR1="#ffffff"
  COLOR2="#666666"
  COLOR3="#333333"
else
  COLOR0="#333333"
  COLOR1="#000000"
  COLOR2="#666666"
  COLOR3="#999999"
fi

state=$(osascript -e '
try 
  tell application "iTunes"
    with timeout 3 seconds
      player state as string
    end timeout
  end tell
on error errText
  "not available"
end try  
');
if [ "$state" = "not available" ]; then
  echo "♫ | size=12"
  echo "---"
  echo "iTunes is not available"
  exit
fi

track=$(osascript -e'
try
tell application "iTunes" to name of current track as string
on error errText
  "no track selected"
end try
');

artist=$(osascript -e'
try
	tell application "iTunes" to artist of current track as string
on error errText
    ""
end try
');

album=$(osascript -e'
try
	tell application "iTunes" to album of current track as string
on error errText
    ""
end try
');

tmp_file=$(osascript -e'
try
    tell application "iTunes"
        tell artwork 1 of current track
            if format is JPEG picture then
                set imgFormat to ".jpg"
		    else
                set imgFormat to ".png"
            end if
	    end tell
        set albumName to album of current track
        set albumArtist to album artist of current track
        if length of albumArtist is 0
            set albumArtist to artist of current track
        end if
        set fileName to (do shell script "echo " & quoted form of albumArtist & quoted form of albumName & " | sed \"s/[^a-zA-Z0-9]//g\"") & imgFormat
    end tell
	(POSIX path of (path to temporary items from user domain)) & fileName
on error errText
    ""
end try
');

if [ ! -f "$tmp_file" ]; then
    osascript -e'
    try
        tell application "iTunes"
            tell artwork 1 of current track
                set srcBytes to raw data
                if format is JPEG picture then
                    set imgFormat to ".jpg"
                else
                    set imgFormat to ".png"
                end if
            end tell
            set albumName to album of current track
            set albumArtist to album artist of current track
            if length of albumArtist is 0
                set albumArtist to artist of current track
            end if
            set fileName to (do shell script "echo " & quoted form of albumArtist & quoted form of albumName & " | sed \"s/[^a-zA-Z0-9]//g\"") & imgFormat
        end tell
        set tmpName to ((path to temporary items from user domain) as text) & fileName
        set outFile to open for access file tmpName with write permission
        set eof outFile to 0
        write srcBytes to outFile
        close access outFile
        tell application "Image Events"
            set resImg to open tmpName
            scale resImg to size 200
            save resImg
            close resImg
        end tell
        tmpName
    on error errText
        ""
    end try
    '
fi

if [ -f "$tmp_file" ]; then
    base64img=$(base64 < "$tmp_file")
fi

if [ "$state" = "playing" ]; then
  state_icon="▶︎"
else
  state_icon="𝝞𝝞"
fi

if [ "$track" != "no track selected" ]; then
    echo "♫ $state_icon $track - $artist | color=$COLOR0 size=12"
else
    echo "♫ ◼︎ | color=$COLOR0 size=12"
fi

echo "---"

if [ "$state" = "playing" ]; then
  echo "𝝞𝝞 Pause | bash='$0 'param1=playpause terminal=false refresh=true color=$COLOR0"
  echo "« Previous | bash='$0' param1=previous terminal=false refresh=true color=$COLOR0"
  echo "» Next | bash='$0' param1=next terminal=false refresh=true color=$COLOR0"
else
  echo "▶︎ Play | bash='$0' param1=playpause terminal=false refresh=true color=$COLOR0"
fi

echo "---"

if [ "$track" != "no track selected" ] && [ "$base64img" != "" ]; then
    echo "| image=$base64img bash='$0' param1=open terminal=false"
fi

if [ "$track" != "no track selected" ]; then
    echo "$track | color=$COLOR1"
    echo "$artist | color=$COLOR2"
    echo "$album | size=12 color=$COLOR3 length=30"
fi

echo '---'
