#!/bin/bash
#  <xbar.title>Mail</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Harrison Page</xbar.author>
#  <xbar.author.github>harrisonpage</xbar.author.github>
#  <xbar.desc>Show unread count from Mail.app in menubar</xbar.desc>
#  <xbar.dependencies>bash,osascript</xbar.dependencies>
#  <xbar.image>https://raw.githubusercontent.com/harrisonpage/unread/main/unread.png</xbar.image>
set -e

OUTPUT=$(osascript -e 'tell Application "Mail"' -e 'unread count of inbox' -e 'end tell')

if [[ $OUTPUT -gt 0 ]]
then
  echo -n "📬"
else
  echo -n "📪"
fi
echo " ${OUTPUT}"
