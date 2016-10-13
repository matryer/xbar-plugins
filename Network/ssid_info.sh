#!/bin/bash
# <bitbar.title>ssid-info</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Örjan Berglin</bitbar.author>
# <bitbar.author.github>orjanb</bitbar.author.github>
# <bitbar.desc>Show the SSID of your current Wi-Fi connection. The SSID is green if the network is secure and red if it is an open network.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/0kHyHnn.png</bitbar.image>

# Get ssid and auth type 
ssid=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}')
auth=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ link auth/ {print substr($0, index($0, $3))}')

color=green
if [ "$auth" = "none" ]; then
	color=red
else
	color=green
fi

echo "$ssid | color=$color "
echo "---"
echo "SSID: $ssid | color=$color"
echo "Auth: $auth | color=$color"
