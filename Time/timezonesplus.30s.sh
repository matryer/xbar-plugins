#!/bin/bash

# <bitbar.title>Timezones+</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Aaron Edell</bitbar.author>
# <bitbar.author.github>aaronedell</bitbar.author.github>
# <bitbar.desc>Rotates current time through four common timezones </bitbar.desc>
# <bitbar.image>http://i.imgur.com/Y4nhdZo.png</bitbar.image>
# <bitbar.dependencies>Bash GNU AWK</bitbar.dependencies>




echo -n "New York " ; TZ=":US/Eastern" date +'%l:%M %p'
echo -n "San Francisco " ; TZ=":US/Pacific" date +'%l:%M %p'
echo -n "London " ; TZ=":Europe/London" date +'%l:%M %p'
echo -n "Hong Kong " ; TZ="Asia/Hong_Kong" date +'%l:%M %p'
