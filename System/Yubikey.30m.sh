#!/bin/bash
#
# <bitbar.title>YubiKey TOTP to clipboard</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>jebabin</bitbar.author>
# <bitbar.author.github>jebabin</bitbar.author.github>
# <bitbar.desc>generates totp codes using yubikey and copy to clipboard, supports touch and touchless code generation</bitbar.desc>
# <bitbar.dependencies>bash,perl</bitbar.dependencies>
# <bitbar.version>1.0</bitbar.version>
# clicking on an item will copy the code to the clipboard

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

if [ $# -eq 3 ] && [ "$1" = "copy" ]; then
  if [ $3 -eq 1 ]; then
	  osascript -e "display notification \"You need to touch your yubikey to get the code\" with title \"$2\"" &> /dev/null
  fi
  code=`"/Applications/YubiKey Manager.app/Contents/MacOS/ykman" oath code "$2" | sed 's/.*[[:space:]]//'`
  echo -n "$code" | pbcopy
  osascript -e "display notification \" $code copied to clipboard\" with title \"$2\"" &> /dev/null
  exit
else
 
  tmpfile=$(mktemp)

  "/Applications/YubiKey Manager.app/Contents/MacOS/ykman" oath code | while read -r line
  do
    parsed=`echo "${line}" | perl -ne 'm/^([^:]+):(.*?)\s+(\d+|\[Touch Credential\])$/; print sprintf(int($3) ? 0 : 1).";$1 ($2)\n"'`
    name=`echo "${parsed}" | cut -c 3-100`
    needtouch=`echo "${parsed}" | cut -c 1`
    account=`echo "${line}" | perl -ne 'm/^(.*?)\s+(\d+|\[Touch Credential\])$/; print $1'`


    echo "$name | font=Menlo size=13 bash='$0' param1=copy param2=\"$account\" param3=$needtouch refresh=true terminal=false" >> $tmpfile
  done

  echo "y| font=Meiryo size=14"
  echo "---"
  cat $tmpfile

  exit
fi


