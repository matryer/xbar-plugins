#!/bin/bash

# <xbar.title>Active WIFI Name</xbar.title>
# <xbar.author>Jiri</xbar.author>
# <xbar.author.github>CzechJiri</xbar.author.github>
# <xbar.desc>Displays currently connected WIFI Name</xbar.desc>

WIFINAME=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport -I | awk '/ SSID/ {print substr($0, index($0, $2))}')

echo "WIFI: $WIFINAME"
