#!/bin/bash

# <bitbar.title>Copy to Clipboard</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Parvez</bitbar.author>
# <bitbar.author.github>parvez</bitbar.author.github>
# <bitbar.desc>This plugin will copy text to clipboard</bitbar.desc>
# <bitbar.image>http://i.imgur.com/SiuAz3C.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# Hack for language not being set properly and unicode support
export LANG="${LANG:-en_US.UTF-8}"

# Write the list of Text you want enabled
LIST="
Text 1
Some Text 2
Some moreText 3
"

if [[ "$1" == "copy" ]]; then
  echo -n "$(echo -n "$2")" | pbcopy
  exit
fi

echo "📋"
echo '---'
echo "Clear Clipboard | bash='$0' param1=copy param2=' ' terminal=false"
echo "---"
while read -r line; do
  if ! [ "$line" == "" ]; then
    echo "$line | bash='$0' param1=copy param2='$line' terminal=false"
  fi
done <<< "$LIST"
