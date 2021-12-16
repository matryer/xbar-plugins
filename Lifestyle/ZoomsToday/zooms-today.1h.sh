#!/bin/bash

#
# Display a list of Zoom based meetings in your calendar today.
# Selecting a meeting will take you to the Zoom session associated with it.
#
# <xbar.title>Zoom Meetings for Today</xbar.title>
# <xbar.version>1.0.0</xbar.version>
# <xbar.author>Luis Cruz</xbar.author>
# <xbar.author.github>sprak3000</xbar.author.github>
# <xbar.desc>Display a list of Zoom based meetings in your calendar today. Selecting a meeting will take you to the Zoom session associated with it.</xbar.desc>
# <xbar.image>https://sprak3000.github.io/assets/images/blog/zooms-today.png</xbar.image>
# <xbar.dependencies>icalBuddy</xbar.dependencies>
#
# Dependencies:
#	  - icalBuddy: https://hasseg.org/icalBuddy/
#   - open-zoom.sh: Should be included with this plugin
#
# Configuration:
#   Set the userdir and plugindir variables below to match your directory structure for BitBar plug-ins.
#

# Add the path iCalBuddy should be installed under.
export PATH="/usr/local/bin:$PATH"

whoami=$(whoami)
userdir="\/Users\/$whoami"

# The location of your BitBar plugin directory.
# It is assumed it is under your user directory.
plugindir=".bitbar"

# Zoom URL pattern
zoomURLPattern="https:\/\/.*.zoom.us\/j\/([0-9]+)"

statusBarIcon=":busts_in_silhouette:"
noZoomsMessage=":tada: No Zoom meetings today! | color=green"

# Grab the calendar events.
events=$(icalBuddy \
		--includeOnlyEventsFromNowOn \
		--noCalendarNames \
		--excludeAllDayEvents \
		--excludeEventProps "attendees" \
		--notesNewlineReplacement " " \
		--bullet "» " \
		--propertySeparators "/ » /" eventsToday | \
	grep -E "$zoomURLPattern" | \
	sed -E "s/^» ([^»]+).*(https:\/\/.*.zoom.us\/j\/[0-9]+).* » (.*)$/\1\3 | bash=$userdir\/$plugindir\/open-zoom.sh param1=\2 terminal=false/")

# Print out the status bar icon and indicate there will be menu items.
echo "${statusBarIcon}"
echo "---"

# Display the events or a message if no events for the day.
if [ -z "$events" ]; then
  echo "${noZoomsMessage}"
else
  echo "${events}"
fi

