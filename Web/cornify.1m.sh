#!/bin/sh
#
# <xbar.title>Unicorn!</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Martin Wilhelmi</xbar.author>
# <xbar.author.github>mnin</xbar.author.github>
# <xbar.desc>Unicorn</xbar.desc>
# <xbar.image>http://i.imgur.com/UMphE3q.png</xbar.image>

TIMESTAMP=$(date +"%s")
IMAGE=$(curl -s -L "https://www.cornify.com/getacorn.php?r=$TIMESTAMP" | base64)

echo '🦄'
echo '---';
echo "| href=https://www.cornify.com/unicornpictures terminal=false image=$IMAGE"
