#!/bin/bash

# Show tasks in your Inbox in Things
#
# by Max Clayton Clowes (maxcc@me.com)
#
# Shows tasks in your Inbox. Find/replace "Inbox" with a list of your choice - e.g "Today"
# 60 second refresh might be too slow. Tweak to your liking.
# Only shows 20 todos - too many stops todos from being completed

# metadata
# <bitbar.title>Things 3 Inbox</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Max Clayton Clowes</bitbar.author>
# <bitbar.author.github>mcclowes</bitbar.author.github>
# <bitbar.desc>Display tasks due Inbox in Things 3.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/2IvhNws.png</bitbar.image>

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
	tellthings "set toDo to to do named \"$2\" of list \"Inbox\"
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

echo "☑"

echo "---"

echo "Inbox"

items=$(tellthings 'set the_list to {}
repeat with n from 1 to count of to dos of list "Inbox"
	set toDo to item n of to dos of list "Inbox"
	set toDoName to name of toDo
		if status of toDo = open then
			set toDoOutput to "☐" & " " & toDoName
		else
			set toDoOutput to "☑" & " " & toDoName
		end if
		set the_list to the_list & toDoOutput
	if n > 20 then
		return the_list
	end if
end repeat
return the_list');

IFS=","
for i in $items; do
	echo "$i | bash='$0' param1='complete' param2='$i' terminal=false"
done

echo "View more... | color=#aaaaaa bash='$0' param1=launch terminal=false"

echo "---"

echo "New to do | bash='$0' param1='show quick entry panel' terminal=false"

echo "Log completed | bash='$0' param1='log completed now' terminal=false"

echo "Empty trash | bash='$0' param1='empty trash' terminal=false"

echo '---'

echo "Open Things 3 | bash='$0' param1=launch terminal=false"
