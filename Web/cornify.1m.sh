#!/bin/sh
#
# <bitbar.title>Unicorn!</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Martin Wilhelmi</bitbar.author>
# <bitbar.author.github>mnin</bitbar.author.github>
# <bitbar.desc>Unicorn</bitbar.desc>
# <bitbar.image>http://i.imgur.com/UMphE3q.png</bitbar.image>

TIMESTAMP=$(date +"%s")
IMAGE=$(curl -s "http://www.cornify.com/getacorn.php?r=$TIMESTAMP" | base64)

echo '🦄'
echo '---';
echo "| href=http://www.cornify.com/unicornpictures terminal=false image=$IMAGE"
