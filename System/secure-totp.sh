#!/usr/bin/env bash

# <xbar.title>Secure TOTP Authenticator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>micxer</xbar.author>
# <xbar.author.github>micxer</xbar.author.github>
# <xbar.desc>This plugin generates TOTP tokens and enables them to be copied to the clipboard. After 30 seconds, the clipboard is automatically emptied again.</xbar.desc>
# <xbar.image>https://i.imgur.com/FelKuk7.png</xbar.image>
# <xbar.dependencies>bash,oathtool</xbar.dependencies>

# update the key value pairs as per your requirement
# Key - for your reference to identify a TOTP Account
# Value - base32 secret key corresponding to the TOTP Account
#  <xbar.var>string(KEYFILE="$HOME/.otp"): Your file with OTP secrets</xbar.var>

KEYFILE="$HOME/.otp"

export LANG="${LANG:-en_US.UTF-8}"

totp_secrets=()
while read -r secret
do
  totp_secrets+=("$secret")
done < "$KEYFILE"

# oath-toolkit needs to be installed. Use 'brew install oath-toolkit' and update
# the path to the oathtool binary below if necessary.
oathtool="/usr/local/bin/oathtool"

function get-totp {
  for secret in "${totp_secrets[@]}" ; do
      KEY="${secret%%:*}"
      VALUE="${secret##*:}"
      if [ "$1" = "$KEY" ]
      then
        $oathtool --totp -b "$VALUE"
        break
      fi
  done
}

if [[ "$1" == "copy" ]]
then
  token="$( get-totp "$2" )"
  echo -n "$token" | pbcopy
  osascript -e "display notification \"TOTP code copied to clipboard!\nIt will be cleared in 30 seconds!\""
  sleep 30s
  echo "" | pbcopy
  osascript -e "display notification \"Clipboard cleared!\""
  exit
fi

# Default Lock icon
echo " | templateImage=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"
echo "---"

for secret in "${totp_secrets[@]}" ; do
    KEY="${secret%%:*}"
    echo "$KEY | shell='$0' param1=copy param2='$KEY' terminal=false"
done

echo '---'
echo "Clear Clipboard | shell='$0' param1=copy param2=' ' terminal=false"
