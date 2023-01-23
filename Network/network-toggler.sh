#!/bin/bash

# <xbar.title>Network Toggler</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>David Shrestha</xbar.author>
# <xbar.author.github>davidshr10</xbar.author.github>
# <xbar.image>iVBORw0KGgoAAAANSUhEUgAAAKEAAABRAQMAAACADVTsAAAABlBMVEUiIiL///9ehyAxAAABrElEQVR4Xu3QL2/bQBgG8NdRlrnMNqxu1eVAahCQVAEF03STbsuBSFVZYEBBoJ2RjZ0Hljuy6IZaUlUlpfsKRUmZP4JTNJixkEm7nJu/Mxlot0l7JJOfXj06P/D3xvkBQH/lqoEC7WVvzqM0k/f4+Gat2nt7ppqeCjCbiJX6HmN7vnca4LLc0BljH/yZ0ZejDQXGlA9GmYSthoumVw1wZ6PByxjrpxmeZq0hbMcDXPCHGVB4hHCAkgUKrrNSulawelPRCH37mu4fR1EdZYPwnTA6UZoQfteoMSmPCFVcgYmUmmCuPMKkIAtNFjqS+hWyOo+MzmVsb12NS1aFazThe1Ztr2qYBklWvcPKCKG+TA/MGwjqDcI4n1Pko+1E5KM9TRz75fGB0qWv1Vlq/Bo9Gzqo3oqu7g991G1bVQmp8IQcdeRtEGpyxoVVB5eNLob0qS6xpaJc5+J7Wx+wkwct5SoSn2vCOORKrHZk0lC69tAbm4a2g0grEuknvd9tb61XhqK8hz+d/xG/cft5fD0dvxA7qsLrj+EXWqBugRbeHl6qcbCr4Ba+7Tn88/kJk4CIztd1IrIAAAAASUVORK5CYII=</xbar.image>
# <xbar.desc>Provides an easy way to toggle your network connections on and off.</xbar.desc>
# <xbar.dependencies>OS X 10.11</xbar.dependencies>

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