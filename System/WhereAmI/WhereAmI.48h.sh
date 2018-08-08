#!/bin/bash

# <bitbar.title>WhereAmI</bitbar.title>
# <bitbar.version>v1</bitbar.version>
# <bitbar.author>noyannus</bitbar.author>
# <bitbar.author.github>noyannus</bitbar.author.github>
# <bitbar.desc>Displays current system volume, system version, and user's home folder. 
# <bitbar.desc>Useful for staying oriented while booting into multiple test systems.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/noyannus/bitbar-plugins/master/System/WhereAmI/Minimalist.png</bitbar.image>
# <bitbar.image>https://raw.githubusercontent.com/noyannus/bitbar-plugins/master/System/WhereAmI/KingSizeInfo.png</bitbar.image>


# Use the last `cut` to get only the relevant part of something
# like "High Sierra beta with Spaghetticode Fork #123 Version 456alpha"
BOOTVOLUME=$(diskutil info "$(df / | tail -1 | cut -d' ' -f 1)" | grep 'Volume Name:' | cut -c  30-)

SYSTEMVERSION=$(defaults read loginwindow SystemVersionStampAsString)

USERDIR=$HOME


# Use either minimalist version, or king size info:

# The minimalist version:
# echo "• $BOOTVOLUME" @ "$SYSTEMVERSION •"

# The king size info:
echo "◥◤"         # if you write a nice way to color this less brutally, send me a pull request.
echo "---"
echo "Boot Volume:        $BOOTVOLUME | size=18"    color=black
echo "System Version:   $SYSTEMVERSION | size=18" color=black
echo "User Directory:     $USERDIR | size=18"       color=black
