#!/bin/bash

# <bitbar.title>Timezones+</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Aaron Edell</bitbar.author>
# <bitbar.author.github>aaronedell</bitbar.author.github>
# <bitbar.desc>Rotates current time through four common timezones </bitbar.desc>
# <bitbar.image>http://i.imgur.com/Y4nhdZo.png</bitbar.image>
# <bitbar.dependencies>Bash GNU AWK</bitbar.dependencies>

echo -n "NY " ; TZ=":US/Eastern" date +'%l:%M %p'
echo -n "SF " ; TZ=":US/Pacific" date +'%l:%M %p'
echo -n "LN " ; TZ=":Europe/London" date +'%l:%M %p'
echo -n "HK " ; TZ="Asia/Hong_Kong" date +'%l:%M %p'
