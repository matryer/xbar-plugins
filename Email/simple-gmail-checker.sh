#!/bin/bash
# <bitbar.title>Simple Gmail Checker</bitbar.title>
# <bitbar.version>v0.1.0</bitbar.version>
# <bitbar.author>Murat Bastas</bitbar.author>
# <bitbar.author.github>murat</bitbar.author.github>
# <bitbar.desc>Checks gmail and displays inbox count + unread mails' subjects</bitbar.desc>
# <bitbar.dependencies>sh,libxml2</bitbar.dependencies>

USERNAME="example@gmail.com"
PASSWORD="verysecretpassword"

RESULT=$(curl -s -u $USERNAME:$PASSWORD "https://mail.google.com/mail/feed/atom" | xmllint --format -)
COUNT=$(echo "$RESULT" | grep -E -o '<fullcount>[0-9]*' | cut -c 12-)
ENTRIES=$(echo "$RESULT" | sed -n 's|<title>\(.*\)</title>|\1|p' | tr -s ' ')

if [ "$COUNT" -ne 0 ]; then
  echo "📬 $COUNT | color=white"
else
  echo "📭"
fi
echo "---"

count=0
echo "$ENTRIES" | while read -r line || [[ -n $line ]];
do
  if [ $count -eq 1 ]; then
    echo "---"
  fi

  echo "$line | href=https://mail.google.com"
  (( count++ ))
done

