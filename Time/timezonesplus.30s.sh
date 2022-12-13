#!/bin/bash

# <xbar.title>Timezones+</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Aaron Edell</xbar.author>
# <xbar.author.github>aaronedell</xbar.author.github>
# <xbar.desc>Rotates current time through four common timezones </xbar.desc>
# <xbar.image>http://i.imgur.com/Y4nhdZo.png</xbar.image>
# <xbar.dependencies>Bash GNU AWK</xbar.dependencies>

echo -n "NY " ; TZ=":US/Eastern" date +'%l:%M %p'
echo -n "SF " ; TZ=":US/Pacific" date +'%l:%M %p'
echo -n "LN " ; TZ=":Europe/London" date +'%l:%M %p'
echo -n "HK " ; TZ="Asia/Hong_Kong" date +'%l:%M %p'
