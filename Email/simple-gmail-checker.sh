#!/bin/bash
# <xbar.title>Simple Gmail Checker</xbar.title>
# <xbar.version>v0.1.0</xbar.version>
# <xbar.author>Murat Bastas</xbar.author>
# <xbar.author.github>murat</xbar.author.github>
# <xbar.desc>Checks gmail and displays inbox count + unread mails' subjects</xbar.desc>
# <xbar.dependencies>sh,libxml2</xbar.dependencies>

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

