#!/bin/bash
#
# <bitbar.title>YubiKey TOTP generator to clipboard plugin</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>reuel</bitbar.author>
# <bitbar.author.github>brute-force</bitbar.author.github>
# <bitbar.desc>generates totp codes using yubikey</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/brute-force/</bitbar.abouturl>
# <bitbar.version>1.0</bitbar.version>
# clicking on an item will copy the code to the clipboard

if [[ "$1"  && "$1" = "copy" && "$2" && "$3" ]]; then
  echo -n "$3" | pbcopy
  osascript -e "display notification \"copied $3 to clipboard\" with title \"$2\"" &> /dev/null
  exit
else
  echo "my codes"
  echo "---"

  /usr/local/bin/ykman oath code | while read -r line
  do
    account=$(echo "$line" | cut -d':' -f1 )
    code=$(echo "$line" | sed 's/.*\([0-9]\{6\}\)$/\1/')

    # align left with padding
    numspaces=$((20-${#account}))
    padding=$(printf "%*s" "$numspaces" "")
    item="$account $padding $code"

    echo "$item | color=orange font=Menlo size=12 bash='$0' param1=copy param2=$account param3=$code refresh=true terminal=false"
  done

  exit
fi
