#!/bin/bash

# <bitbar.title>Active WIFI Name</bitbar.title>
# <bitbar.author>Jiri</bitbar.author>
# <bitbar.author.github>CzechJiri</bitbar.author.github>
# <bitbar.desc>Displays currently connected WIFI Name</bitbar.desc>

WIFINAME=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}')

echo "WIFI: $WIFINAME"
