#!/bin/bash

# <bitbar.title>Network Toggler</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>David Shrestha</bitbar.author>
# <bitbar.author.github>davidshr10</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/QRNTmet.png</bitbar.image>
# <bitbar.desc>Provides an easy way to toggle your network connections on and off.</bitbar.desc>
# <bitbar.dependencies>OS X 10.11</bitbar.dependencies>

if [ "$2" == 'toggle_on' ]; then
	networksetup -setnetworkserviceenabled "$1" on
elif [ "$2" == 'toggle_off' ]; then
	networksetup -setnetworkserviceenabled "$1" off
fi

NETWORK_INTERFANCES=$(networksetup -listallnetworkservices |\
awk '/disabled\./,EOF { if(NR>1) print $0 }')

network_icon="iVBORw0KGgoAAAANSUhEUgAAAB0AAAAZCAYAAADNAiUZAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAACXBIWXMAAAsTAAALEwEAmpwYAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgpMwidZAAADwklEQVRIDb1WTUhUURi9788ZmxlDyp3GDE0LsSQRKSjJdWDawnVSQslALaUgmEVtWlpZkiEVROBCwmWLXLQRM4MaKDQYHBeWOII/zeS8n855vju8Mady4Ty43nu/e75z7v2+795RSSaFmkr1KKJC39jYmKP09PRoy8vLFROtq6tz1AodsCjT1NRUeVGq68Ut7DJA2IuRQFjsXSAlpv/FlxUlQSqV2pKsHR0dZbES48cjjFXlNlo8iXRkn06ndRLoun5OVdWnmqadn5ycNNfX13ctONq5Thzx9KP/4uKi5ueV4xJRVhYF0fKGYbRYlvXEcZzLaN1w0CKRiLNTmHPauS6Ec5F4+tF/fn7+F/nIKwXZF0W5MD09bVCwurq6wTTNIawfQ/uME3BsMeSegJDinHu5tFRVe0g8/eD/iDzkI69f2BX1C4LkcD6fvw/H00IoGQj2FwqFj62trQZs7kfBzs5OSwrTyBwSRzymC2inwDNIvj+E+TjE4/EAHdlrmjqiKIqDtllVVcWwimhUBNG5GM69LyQHXh/wcAJ+XfQnD3I84udPJpOqYFVywJ2iAO54gj8BvkqympqaODYyihO8CIfDjTApyFefpilv2GOuQqQR+OfAjALPlAjMr0lh8N4lP3XcWyB3UVtbewbYDYrC+VU0GuXphK6rN72NcNe3YYph/sWzfeWcdm/uQOAW/ehPnm272AT/WdrdaDY3Nyu9vb0ik8ksra6uBlF97WhH19bWvqN/Hw5HllAUNYoiPoFwGJX5zTCCOdTVAURg2Ladt6FQ6AfsB8H5AZjHGGfhfwX+N2AzNE2/F4vFXiYSCSebzQqxM6fYtT+nXdzdXnMaCGgXcEI3auST0WTPELuvTEtLCw5jBnGv8qi2gY2NjUPQ6kY1DmLnK+m0+Y45gc19ZbzqzU1MTBjyygSDQXtmZqYAfPvWlvUAWBbaa0RhgPeV4YZOAbbte4p7prS1tRW4AMIVEFzH2hTaEYRqCAVz0v/EUQiC7mNBEn4UJM62Ld7pBpxwijzkIy/5qUNs8XHwC+dyuQx2zPs2h3YCRAn0Gu6zLe8mhWFzHwnauY4NJhxHHMd4DqL94FnYKUifkkecwgCZEAwgJLPo+2zbvqTryrhlCYt4KUZnfpx7GzGRx3GYoKc+Q2pmmcP6+npTntB1wJ+y/zkAuOdfGT76kpg14EVAmtwe99QuOal/lQ5e8UjzP39P/fjdBCVRWVEC/uYoCfz9/+KLheR33u9xxUVx9RTeGxWvkoJ8lPzQ7sdpKcj/e38DDOjnoklrR+8AAAAASUVORK5CYII="
echo "|templateImage=$network_icon"
echo "---"

while read -r line; do
    echo "$line"
    if [ "${line:0:1}" == '*' ]; then
    	interface="${line:1}"
    	echo "-- Toggle On | bash='$0' param1='$interface' param2='toggle_on' terminal=false refresh=true color=#3ab24c"
    else
    	echo "-- Toggle Off | bash='$0' param1=$line param2='toggle_off' terminal=false refresh=true color=#d65342"
    fi
done <<< "$NETWORK_INTERFANCES"

echo "---"
echo "Network Preferences | bash='$0' param1=launch-system-preferences terminal=false refresh=true"
if [ "$1" = 'launch-system-preferences' ]; then
  open /System/Library/PreferencePanes/Network.prefPane
fi