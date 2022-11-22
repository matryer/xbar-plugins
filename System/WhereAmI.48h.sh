#!/bin/bash

# <xbar.title>WhereAmI</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>noyannus</xbar.author>
# <xbar.author.github>noyannus</xbar.author.github>
# <xbar.desc>Displays current system volume, system version, and user's home folder.
# <xbar.desc>Useful for staying oriented while booting into multiple test systems.</xbar.desc>
# <xbar.desc>Readme: https://github.com/noyannus/WhereAmI-Readme/blob/master/Readme.md</xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.image>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</xbar.image>


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



