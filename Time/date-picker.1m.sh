#!/bin/bash

# Display todays date and time in various formats including ISO8601 and allows copying to clipboard.

# Comment out the dates you don't need.

# <bitbar.title>Date Picker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tim Battersby</bitbar.author>
# <bitbar.author.github>uglygus</bitbar.author.github>
# <bitbar.desc>Display todays date in various forms including iso8601 and copies to the clipboard.</bitbar.desc>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/GVSUqFX.png</bitbar.image>

# Appears in the menubar YYYY-MM-DD
date +%F
echo "---"

#---ISO8601

YMD=$(date +%F)
echo "$YMD | bash=$0 param1=copy param2=$YMD terminal=false"

DATETIME=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
echo "$DATETIME | bash=$0 param1=copy param2=$DATETIME terminal=false"

echo "---"

#---USA

MDY=$(date "+%D")
echo "$MDY | bash=$0 param1=copy param2=$MDY terminal=false"

 MonDY=$(date +"%b %d %Y")
 echo "$MonDY | bash=$0 param1=copy param2=\"$MonDY\" terminal=false"

TIME12=$(date +"%r")
echo "$TIME12 | bash=$0 param1=copy param2=\"$TIME12\" terminal=false"

echo "---"


#---REST OF THE WORLD

DMY=$(date +"%d/%m/%y")
echo "$DMY | bash=$0 param1=copy param2=$YMD terminal=false"

DMonY=$(date +"%d %b %Y")
echo "$DMonY | bash=$0 param1=copy param2=\"$DMonY\" terminal=false"

TIMESTAMP=$(date +"%T %D")
echo "$TIMESTAMP | bash=$0 param1=copy param2=\"$TIMESTAMP\" terminal=false"


TIME24=$(date +"%R:%S")
echo "$TIME24 | bash=$0 param1=copy param2=\"$TIME24\" terminal=false"



if [[ "$#" -ge 1 ]];then
    if [[ "$1" == 'copy' ]] ; then

        echo -n "$2" | pbcopy
        echo COPIED "$2"
    fi
fi
