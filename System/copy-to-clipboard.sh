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
Some text#I am some text that will be copied to your clipboard
A password2#super_secure_password
A common command#/maybe/I/am/a/command/you/use/often -with -some -params
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
  	title=$(echo $line | cut -f1 -d#)
	  command=$(echo $line | cut -f2 -d#)
    echo "$title | bash='$0' param1=copy param2='$command' terminal=false"
  fi
done <<< "$LIST"
