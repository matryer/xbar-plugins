#!/bin/bash

# <xbar.title>Waketime</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>Matthias Vogt</xbar.author>
# <xbar.author.github>matthias-vogt</xbar.author.github>
# <xbar.desc>Shows the time since your mac last woke up in hours:minutes</xbar.desc>
# <xbar.image>http://i.imgur.com/jsB66g9.png</xbar.image>

wake=$(
	sysctl -a | grep 'waketime' | grep -o "\d\{10\}" ||
	sysctl -a | grep 'boottime' | grep -o "\d\{10\}"
);
now=$(date +'%s');

hours=$(echo "($now - $wake)/3600" | bc)
minutes=$(echo "(($now - $wake)/60)%60" | bc)

echo "$hours:$(printf "%02d" "$minutes")h"
