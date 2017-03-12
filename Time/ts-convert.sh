#!/bin/bash

# <bitbar.title>Clipboard Timestamp converter</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>M. Peter</bitbar.author>
# <bitbar.author.github>mpneuried</bitbar.author.github>
# <bitbar.desc>This plugin converts timestamps from the clipboard to a date.
# It's inspired by the original clipboard-base64-encoder plugin</bitbar.desc>
# <bitbar.image>http://i.imgur.com/dXzJNOS.jpg?1</birtbar.image>
# <bitbar.dependencies></bitbar.dependencies>

# Hack for language not being set properly and unicode support
export LANG="${LANG:-en_US.UTF-8}"

# user clicks on 'encodepaste'
if [[ "$1" = "encodepaste" ]]; then
  CONVERTED=$(date -r "$(pbpaste)" +'%Y-%m-%d %H:%M:%S')
  echo -n "$CONVERTED" | pbcopy
  osascript -e "display notification \"$CONVERTED\" with title \"DATE:\"" &> /dev/null
  exit
fi

# user clicks on 'encoding'
if [[ "$1" = "encode" ]]; then
  CONVERTED=$(date -r "$(pbpaste)" +'%Y-%m-%d %H:%M:%S')
  osascript -e "display notification \"$CONVERTED\" with title \"DATE:\"" &> /dev/null
  exit
fi

# user clicks on 'now'
if [[ "$1" = "now" ]]; then
  NOW=$(date +%s)
  echo -n "$NOW" | pbcopy
  osascript -e "display notification \"$NOW\" with title \"Save to Clipboard:\"" &> /dev/null
  exit
fi

# Print icon
echo "⏱"
echo "---"
echo "Convert clipboard timestamp to date | bash='$0' param1=encode terminal=false"
echo "Convert clipboard timestamp to date and write it to the clipboard | bash='$0' param1=encodepaste terminal=false"
echo "Write the current timestamp to the clipboard | bash='$0' param1=now terminal=false"
