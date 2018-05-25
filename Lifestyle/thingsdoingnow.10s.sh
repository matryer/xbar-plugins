#!/bin/bash

# Show the one task you tagged with `Now` in Things
#
# by Florent Crivello (forked from Max Clayton Clowes' plugin (maxcc@me.com))
#

# metadata
# <bitbar.title>Things Doing Now</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Florent Crivello</bitbar.author>
# <bitbar.author.github>altimor</bitbar.author.github>
# <bitbar.desc>Show the one task you tagged with `Now` in Things</bitbar.desc>
# <bitbar.image>http://i.imgur.com/CLtYE1E.gif</bitbar.image>

function tellthings() {
	osascript -e "tell application \"Things3\" 
	$1
end tell"
}

if [ "$1" = 'launch' ]; then
  tellthings 'activate'
  exit
fi

case "$1" in
  'show quick entry panel' | 'log completed now' | 'empty trash')
    tellthings "$1"
    exit
esac

if [ "$1" = 'complete' ]; then
  tellthings "set toDo to to do named \"$2\" of list \"Today\"
	set status of toDo to completed
	delay 1.3"
  exit
fi

if [ "$(osascript -e 'application "Things3" is running')" = "false" ]; then
  echo "☑"
  echo "---"
  echo "Things 3 is not running"
  echo "Launch Things3 | bash='$0' param1=launch terminal=false"
  exit
fi

currentlyDoing=$(tellthings 'set currentlyDoing to ""
repeat with n from 1 to count of to dos of list "Today"
	set toDo to item n of to dos of list "Today"
	set toDoName to name of toDo
	if status of toDo = open then
    set tagList to tags of toDo 
    repeat with aTag in tagList
    set tagName to name of aTag
      if tagName = "Now" then
        set currentlyDoing to toDoName
        return currentlyDoing
      end if
      end repeat
	end if
end repeat
return currentlyDoing');

if [ "$currentlyDoing" != "" ]; then
	echo "☐ $currentlyDoing | bash='$0' param1=launch terminal=false"
fi