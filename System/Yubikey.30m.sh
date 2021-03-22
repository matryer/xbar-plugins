#!/bin/bash
#
# <bitbar.title>YubiKey TOTP to clipboard</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>jebabin</bitbar.author>
# <bitbar.author.github>jebabin</bitbar.author.github>
# <bitbar.desc>generates totp codes using yubikey and copy to clipboard, supports touch and touchless code generation</bitbar.desc>
# <bitbar.dependencies>bash,perl</bitbar.dependencies>
# <bitbar.version>1.0</bitbar.version>

# <xbar.var>string(VAR_YKMAN): Path to the ykman binary (optional).</xbar.var>

# v1.1
# Put the menu bar icon un green when yubikey is present
# Put a message when yubikey need to be inserted
# Remove temporary file
# Add a variable to set ykman path and try to auto detect if not set
# If a file with <issuer name>.b64 (an image file encoded as base64 - height of 32px recommended) is present in Yubikey subdirectory, it will be displayed in the menu)

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

if [ ! -x "$VAR_YKMAN" ]; then
	if [ ! -x "/Applications/YubiKey Manager.app/Contents/MacOS/ykman" ]; then
		VAR_YKMAN="/Applications/YubiKey Manager.app/Contents/MacOS/ykman"
	else
		VAR_YKMAN=ykman
	fi
fi

if [ $# -eq 3 ] && [ "$1" = "copy" ]; then
	if [ "$3" -eq 1 ]; then
		osascript -e "display notification \"You need to touch your yubikey to get the code\" with title \"$2\"" &> /dev/null
	fi
	code=$("/Applications/YubiKey Manager.app/Contents/MacOS/ykman" oath code "$2" | sed 's/.*[[:space:]]//')
	echo -n "$code" | pbcopy
	osascript -e "display notification \" $code copied to clipboard\" with title \"$2\"" &> /dev/null

	exit
else
 
	tmpfile=$(mktemp)

	"/Applications/YubiKey Manager.app/Contents/MacOS/ykman" oath code | while read -r line
	do
		parsed=$(echo "${line}" | perl -ne 'm/^([^:]+):(.*?)\s+(\d+|\[Touch Credential\])$/; print sprintf(int($3) ? 0 : 1).";$1 ($2)\n"')
		name=$(echo "${parsed}" | cut -c 3-100)
		needtouch=$(echo "${parsed}" | cut -c 1)
		account=$(echo "${line}" | perl -ne 'm/^(.*?)\s+(\d+|\[Touch Credential\])$/; print $1')
		issuer=$(echo "${line}" | perl -ne 'm/^([^:]+):(.*?)\s+(\d+|\[Touch Credential\])$/; print "$1"')

		addimage=""
		if [ -e "Yubikey/$issuer.b64" ]; then
			addimage="image="$(cat "Yubikey/$issuer.b64")
		fi
		
		echo "$name | $addimage font=Menlo size=13 bash='$0' param1=copy param2=\"$account\" param3=$needtouch terminal=false" >> "$tmpfile"
	done

	color="#008000"
	if [ ! -s $tmpfile ]; then
		echo "Please insert your yubikey then select this line to refresh | font=Menlo size=13 refresh=true terminal=false" >> "$tmpfile"
		color="black"
	fi 

	echo "y| font=Meiryo size=14 color=$color"
	echo "---"
	cat "$tmpfile"
	rm "$tmpfile"

	exit
fi


