#!/bin/sh

# <xbar.title>Gmail Checker</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>Nadav Cohen</xbar.author>
# <xbar.author.github>nadavc</xbar.author.github>
# <xbar.desc>Checks gmail and displays inbox count</xbar.desc>
# <xbar.image>http://i.imgur.com/LYrIphK.png</xbar.image>

USERNAME="GMAIL_USERNAME"
PASSWORD="GMAIL_PASSWORD"
COLOR=black
RESULT=$(curl -s -u $USERNAME:$PASSWORD "https://mail.google.com/mail/feed/atom" | egrep -o '<fullcount>[0-9]*' | cut -c 12-)

if [ "$RESULT" -ne "0" ]; then
   COLOR=red
fi

echo "📬 $RESULT|color=$COLOR"
