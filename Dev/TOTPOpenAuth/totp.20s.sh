#!/bin/bash

# <bitbar.title>Authenticator</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Gunasekaran Namachivayam</bitbar.author>
# <bitbar.author.github>gunasekar</bitbar.author.github>
# <bitbar.desc>This plugin will generate the TOTP tokens and allows to copy them to clipboard</bitbar.desc>

# Hack for language not being set properly and unicode support
export LANG="${LANG:-en_US.UTF-8}"

# update the key value pairs as per your requirement
# Key - for your reference to identify a TOTP Account
# Value - base32 secret key corresponding to the TOTP Account
vpnsecrets=( "OpenVPN:2b2drladcdoxtpheuom6t4zjsr6tq7ix"
        "BitBucket:2b2drladcdoxtpheuom6t4zjsr6tq7ix"
        "GitHub:2b2drladcdoxtpheuom6t4zjsr6tq7ix"
        "Okta:2b2drladcdoxtpheuom6t4zjsr6tq7ix"
        "GrabTaxi:2b2drladcdoxtpheuom6t4zjsr6tq7ix" )

# oath-toolkit needs to be installed. Use 'brew install oath-toolkit'
# update the appropriate path of oathtool binary in the below function
function get-totp {
  echo "$( /usr/local/Cellar/oath-toolkit/2.6.2/bin/oathtool --totp -b "$1" )"
}

if [[ "$1" == "copy" ]]; then
  echo -n "$(echo -n "$2")" | pbcopy
  exit
fi

echo "⏳"
echo '---'
echo "Clear Clipboard | bash='$0' param1=copy param2=' ' terminal=false"
echo "---"

for secret in "${vpnsecrets[@]}" ; do
    KEY="${secret%%:*}"
    VALUE="${secret##*:}"
    token=$( get-totp "$VALUE" )
    echo "$KEY | bash='$0' param1=copy param2='$VALUE' terminal=false"
done
