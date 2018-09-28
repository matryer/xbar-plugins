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

network_icon="iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAYAAAAeP4ixAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAAFiUAABYlAUlSJPAAAAD2SURBVGhD7ZhRDoJADETxXN5DD+Ph/OBgukPoB42YXeh0N2Re0kBIszOPyI+TEEIIIcR1uK9XBsyzN7zKfNZrNMyzN1iQTWQg8+wNPig6jH3+QkpIgZqTJWFQ8rIljNDcXhJGSH5vCeNUj1EkjEN9RpMwmnrVLt/KPMvM6+AezzzRe1X9aiUAgvzuo4wneg/87dkiAd5l/D6eeaL3jN2+lxEBLTLD/rSMWhl8iAjCW8Pgfu8jjtyr7bfQtJzIoV6jyZzqM4pMSI/eMqH5vWQoudky1LwsmZQcdkiKhOHDmCI0CcMCGUHMs39yib9MhRBCCCGoTNMX1Vc8LLB3HSAAAAAASUVORK5CYII="
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