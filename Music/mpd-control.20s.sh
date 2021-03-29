#!/bin/bash

# <xbar.title>mpd-control</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Slamet Kristanto</xbar.author>
# <xbar.author.github>drselump14</xbar.author.github>
# <xbar.desc>MPD control (mopidy spotify, mopidy soundcloud , etc)</xbar.desc>
# <xbar.image>https://www.dropbox.com/s/cgkjb7hv6s1yx1a/Screenshot%202017-02-08%2020.40.02.png?raw=1</xbar.image>
# <xbar.dependencies>mpd, mpc</xbar.dependencies>

count="$( /usr/local/bin/mpc | wc -l )"

if [ "$count" -gt 2 ]
then
  echo "$(/usr/local/bin/mpc | head -1) | bash='/usr/local/bin/mpc' param1=toggle terminal=false length=25 size=12"
else
  echo ""
fi
