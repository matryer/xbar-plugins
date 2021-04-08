#!/bin/bash
#
# <xbar.title>YubiKey TOTP to clipboard</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>jebabin</xbar.author>
# <xbar.author.github>jebabin</xbar.author.github>
# <xbar.desc>generates totp codes using YubiKey and copy to clipboard, supports touch and touchless code generation</xbar.desc>
# <xbar.dependencies>bash,perl</xbar.dependencies>
# <xbar.version>1.2</xbar.version>

# <xbar.var>string(VAR_YKMAN): Path to the ykman binary (optional).</xbar.var>

# v1.2
# Better handle ykman return code
# Now automatically create and update the .b64 image when a .png, .jpg, etc.. exist in YubiKey directory for the issuer
# Directory changed from Yubikey to YubiKey (uppercase K)
# You can set http link to open for each issue with the `defaults write com.xbarapp.plugins.yubikey "issuer" "https://..."` command in terminal

# v1.1
# Put the menu bar icon un green when YubiKey is present
# Put a message when YubiKey need to be inserted
# Remove temporary file
# Add a variable to set ykman path and try to auto detect if not set
# If a file with <issuer name>.b64 (an image file encoded as base64 - height of 32px recommended) is present in YubiKey (uppercase Y and K!) subdirectory, it will be displayed in the menu)

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

if [ ! -x "$VAR_YKMAN" ]; then
	if [ -x "/Applications/YubiKey Manager.app/Contents/MacOS/ykman" ]; then
		VAR_YKMAN="/Applications/YubiKey Manager.app/Contents/MacOS/ykman"
	else
		VAR_YKMAN=ykman
	fi
fi

if [ $# -eq 3 ] && [ "$1" = "copy" ]; then
	if [ "$3" -eq 1 ]; then
		osascript -e "display notification \"You need to touch your YubiKey to get the code\" with title \"$2\"" &> /dev/null
	fi
	code=$("$VAR_YKMAN" oath code "$2" | sed 's/.*[[:space:]]//')
	if [ "$code" -ge 0 ]; then
		echo -n "$code" | pbcopy
		osascript -e "display notification \" $code copied to clipboard\" with title \"$2\"" &> /dev/null
	else
		osascript -e "display notification \" $code You were too long to touch the key, please retry\" with title \"$2\"" &> /dev/null
	fi
	exit
else

	tmpfile=$(mktemp)

	if [ -d "YubiKey" ]; then
		cd "YubiKey" || exit
		for file in *; do
			extension="${file##*.}"
			filename="${file%.*}"
			if [ "$extension" != "b64" ]; then
				if [ -f "$filename.b64" ]; then
					if [ "$file" -nt "$filename.b64" ]; then
#						echo "$file is newer than the b64, updating b64 file" >> "$tmpfile"
						cat $file | base64 > "$filename.b64"
					fi
				else
#					echo "no b64 file for $file, creating b64 file" >> "$tmpfile"
					cat $file | base64 > "$filename.b64"
				fi
			fi
		done
		cd ".."
	fi

	"$VAR_YKMAN" oath code | while read -r line
	do
		parsed=$(echo "${line}" | perl -ne 'm/^([^:]+):(.*?)\s+(\d+|\[Touch Credential\])$/; print sprintf(int($3) ? 0 : 1).";$1 ($2)\n"')
		name=$(echo "${parsed}" | cut -c 3-100)
		needtouch=$(echo "${parsed}" | cut -c 1)
		account=$(echo "${line}" | perl -ne 'm/^(.*?)\s+(\d+|\[Touch Credential\])$/; print $1')
		issuer=$(echo "${line}" | perl -ne 'm/^([^:]+):(.*?)\s+(\d+|\[Touch Credential\])$/; print "$1"')

		addimage=""
		if [ -e "YubiKey/$issuer.b64" ]; then
			addimage="image="$(cat "YubiKey/$issuer.b64")
		fi
		addurl=""
		URL=$(defaults read com.xbarapp.plugins.yubikey "$issuer")

		echo "$name | $addimage href=$URL font=Menlo size=13 bash='$0' param1=copy param2=\"$account\" param3=$needtouch terminal=false" >> "$tmpfile"
	done

	color="#008000"
	if [ ! -s "$tmpfile" ]; then
		"$VAR_YKMAN" oath code >> /dev/null
		if [ "$?" -eq 2 ]; then
			echo "Please insert your YubiKey then select this line to refresh | font=Menlo size=13 refresh=true terminal=false" >> "$tmpfile"
		else
			echo "Your YubiKey has probably no OATH entry configured | font=Menlo size=13 terminal=false" >> "$tmpfile"
		fi
		color="black"
	fi 

	echo "y| font=Meiryo size=14 color=$color"
	echo "---"
	cat "$tmpfile"
	rm "$tmpfile"

	exit
fi


