#!/bin/bash

###
#
# Make sure you take care of the <bitbar.dependencies>, then
#
# CHANGE THESE
#

readonly path="/Applications/"
readonly processName="aircast-osx-multi"

#
# NOTE: These values cannot contain single or double quotation marks.
# Would love to know how to get BitBar to parse these correctly!
# (Incidentally, |href also chokes on spaces.)
#
###



# Info

# <bitbar.title>AirConnect</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>CartoonChess</bitbar.author>
# <bitbar.author.github>cartoonchess</bitbar.author.github>
# <bitbar.desc>Launches AirConnect to enable streaming AirPlay audio to Chromecast devices.</bitbar.desc>
# <bitbar.image>https://user-images.githubusercontent.com/43363630/93159526-5214f580-f749-11ea-9b1e-0f5a143643d6.png</bitbar.image>
# <bitbar.dependencies>airconnect</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/philippe44/AirConnect</bitbar.abouturl>



# Strings

readonly app="AirConnect"



# Icons

readonly onIcon=$(cat '/System/Library/CoreServices/Menu Extras/IrDA.menu/Contents/Resources/IRDA3ConnectedCropped.pdf' | openssl base64 | tr -d '\n')
readonly offIcon=$(cat '/System/Library/CoreServices/Menu Extras/IrDA.menu/Contents/Resources/IRDA1IdleCropped.pdf' | openssl base64 | tr -d '\n')



# Run app

if [ "$1" = "start" ]; then
	# Must use -Z to keep CPU under control
	# & runs in background so plugin can refresh
	"$path$processName" -Z &
	exit
fi



# Exit app

if [ "$1" = "stop" ]; then
	pid=$(pgrep "$processName")
	kill -s $2 $pid
	# Give the plugin time to wait for the app process to end
	sleep 1
	exit
fi



# Check if app script is running

running=false
ps cax | grep aircast-osx-multi > /dev/null

if [ $? = 0 ]; then
	running=true
fi



# Show menu bar item

if [ "$running" = true ]; then
	echo "| templateImage=$onIcon"
else
	echo "| templateImage=$offIcon"
fi
echo "---"



# Check that the app can be located and only show help if not

test -f "$path$processName"

if [ $? != 0 ]; then
	echo "$app Not Found"
	echo "Refresh | refresh=true"
	echo "Help"
	echo "-- Set path and filename in .sh file"
	echo "-- Open Plugin Folder… | href=file://${0%/*}/"
	echo "-----"
	echo "-- Download and install $app binary"
	echo "-- Open GitHub Page… | href=https://github.com/philippe44/AirConnect"
	exit
fi




# Show full menu (if app is properly located)

if [ "$running" = true ]; then
	# Running
	echo "Running"
	echo "Stop $app | bash='$0' param1=stop param2=TERM terminal=false refresh=true"
	# Hold opt key to force quit
	echo "Force Quit $app | alternate=true bash='$0' param1=stop param2=KILL terminal=false refresh=true"
else
	# Not running
	echo "Not Running"
	echo "Start $app | bash='$0' param1=start terminal=false refresh=true"
fi