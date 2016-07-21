#!/bin/bash

# <bitbar.title>Waketime</bitbar.title>
# <bitbar.version>v1.0.1</bitbar.version>
# <bitbar.author>Matthias Vogt</bitbar.author>
# <bitbar.author.github>matthias-vogt</bitbar.author.github>
# <bitbar.desc>Shows the time since your mac last woke up in hours:minutes</bitbar.desc>
# <bitbar.image>http://i.imgur.com/jsB66g9.png</bitbar.image>

wake=$(
	sysctl -a | grep 'waketime' | grep -o "\d\{10\}" ||
	sysctl -a | grep 'boottime' | grep -o "\d\{10\}"
);
now=$(date +'%s');

hours=$(echo "($now - $wake)/3600" | bc)
minutes=$(echo "(($now - $wake)/60)%60" | bc)

echo "$hours:$(printf "%02d" "$minutes")h"
