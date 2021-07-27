#!/usr/bin/env bash

# <xbar.title>location-switcher</xbar.title>
# <xbar.author>Aaron Ellington</xbar.author>
# <xbar.author.github>aaronellington</xbar.author.github>
# <xbar.desc>Shows current Network Location and allows you to change it.</xbar.desc>

LOCATION_CURRENT=$(scselect | grep "^ \*" | grep -o "(.*)" | sed 's/(//' | sed 's/)//')

echo "Location: $LOCATION_CURRENT"

echo "---"

scselect | grep -o "(.*)" | grep -v "* == current set" | sed 's/(//' | sed 's/)//' | while read x; do
    echo "Set location: $x | shell=scselect | param1=\"$x\" | refresh=true";
done
