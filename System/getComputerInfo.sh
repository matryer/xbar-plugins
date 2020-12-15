#!/bin/bash

# <bitbar.title>Get Computer Info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sarah Keenan</bitbar.author>
# <bitbar.author.github>SKeenan07</bitbar.author.github>
# <bitbar.desc>This plugin gets the IP address and the computer name.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/SKeenan07/portfolio/master/images/BitBarPlugin.png?raw=true</bitbar.image>

activeNetworkAdapter=$(echo 'show State:/Network/Global/IPv4' | scutil | grep PrimaryInterface | sed 's/ PrimaryInterface : //')

computerName=$(system_profiler SPSoftwareDataType | grep "Computer Name")

batteryCondition=$(system_profiler SPPowerDataType | grep "Condition")

echo "🖥"

echo "---"

echo "${computerName:21}"

echo "---"

# Batery conditions can be Normal, Replace Soon, Replace Now, and Service Battery
echo "Battery Condition: ${batteryCondition:21}"

echo "---"

# If the active network adapter is empty, then the Mac isn't connected to the internet
if [[ -z "$activeNetworkAdapter" ]]; then
    echo "Not connected to the internet."
else
    IPAddress=$(ifconfig $activeNetworkAdapter | grep "inet " | awk '{ print $2 }')
    echo "$IPAddress"
fi
