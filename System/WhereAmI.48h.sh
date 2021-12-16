#!/bin/bash

# <xbar.title>WhereAmI</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>noyannus</xbar.author>
# <xbar.author.github>noyannus</xbar.author.github>
# <xbar.desc>Displays current system volume, system version, and user's home folder.
# <xbar.desc>Useful for staying oriented while booting into multiple test systems.</xbar.desc>
# <xbar.desc>Readme: https://github.com/noyannus/WhereAmI-Readme/blob/master/Readme.md</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/noyannus/WhereAmI-Readme/master/Minimalist.pngg</xbar.image>
# <xbar.image>https://raw.githubusercontent.com/noyannus/WhereAmI-Readme/master/KingSizeInfo.png</xbar.image>


# Use the last `cut` to get only the relevant part of something like "High Sierra beta with Spaghetticode Fork #123 Version 456alpha"
BOOTVOLUME=$(diskutil info "$(df / | tail -1 | cut -d' ' -f 1)" | grep 'Volume Name:' | cut -c  30-)

SYSTEMVERSION=$(defaults read loginwindow SystemVersionStampAsString)

USERDIR=$HOME


# Un/comment below to select appearance: either minimalist version, or king size info

# # The minimalist version:
# echo "• $BOOTVOLUME" @ "$SYSTEMVERSION •"

# The king size info:
echo "◥◤"         # if you write a nice way to color this less brutally, send me a pull request.
echo "---"
echo "Boot Volume:        $BOOTVOLUME | size=18"    color=black
echo "System Version:   $SYSTEMVERSION | size=18" color=black
echo "User Directory:     $USERDIR | size=18"       color=black



