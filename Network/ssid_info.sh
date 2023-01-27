#!/bin/bash
# <xbar.title>ssid-info</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Örjan Berglin</xbar.author>
# <xbar.author.github>orjanb</xbar.author.github>
# <xbar.desc>Show the SSID of your current Wi-Fi connection. The SSID is green if the network is secure and red if it is an open network.</xbar.desc>
# <xbar.image>http://i.imgur.com/0kHyHnn.png</xbar.image>

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
