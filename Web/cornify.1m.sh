#!/bin/sh
#
# <xbar.title>Unicorn!</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Martin Wilhelmi</xbar.author>
# <xbar.author.github>mnin</xbar.author.github>
# <xbar.desc>Unicorn</xbar.desc>
# <xbar.image>http://i.imgur.com/UMphE3q.png</xbar.image>

TIMESTAMP=$(date +"%s")
IMAGE=$(curl -s "http://www.cornify.com/getacorn.php?r=$TIMESTAMP" | base64)

echo '🦄'
echo '---';
echo "| href=http://www.cornify.com/unicornpictures terminal=false image=$IMAGE"
