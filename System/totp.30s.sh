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

if [ $# -eq 3 ] && [ "$1" = "copy" ]; then
  echo -n "$3" | pbcopy
  osascript -e "display notification \" $3 copied to clipboard\" with title \"$2\"" &> /dev/null
  exit
else
  echo "TOTP"
  echo "---"

  /usr/local/bin/ykman oath code | while read -r line
  do
    account=${line/%:* *[0-9]/}
    code=${line##* }

    # trim account name to max characters
    account_length_max=20
    account=${account:0:account_length_max}

    # pad code to for alignment
    code_length_max=6
    width=$((account_length_max - ${#account} + code_length_max))
    item=$(printf "%s %*s" "$account" $width "$code")

    echo "$item | color=green font=Menlo size=13 bash='$0' param1=copy param2=\"$account\" param3=$code refresh=true terminal=false"
  done

  exit
fi
