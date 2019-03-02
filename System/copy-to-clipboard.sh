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

echo "📋"
echo '---'
echo "Clear Clipboard | bash='$0' param1=copy param2=' ' terminal=false"
echo "---"
while read -r line; do
  if ! [ "$line" == "" ]; then
    echo "$line | bash='/bin/bash' param1='-c' param2='/bin/echo $line | pbcopy' terminal=false"
  fi
done <<< "$LIST"
