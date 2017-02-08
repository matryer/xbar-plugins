#!/bin/bash

# <bitbar.title>mpd-control</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Slamet Kristanto</bitbar.author>
# <bitbar.author.github>drselump14</bitbar.author.github>
# <bitbar.desc>MPD control (mopidy spotify, mopidy soundcloud , etc)</bitbar.desc>
# <bitbar.dependencies>mpd, mpc</bitbar.dependencies>

count="$( /usr/local/bin/mpc | wc -l )"

if [ "$count" -gt 2 ]
then
  echo "$(/usr/local/bin/mpc | head -1) | bash='/usr/local/bin/mpc' param1=toggle terminal=false length=25 size=12"
else
  echo ""
fi
