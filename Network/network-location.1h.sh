#!/bin/bash
# <xbar.title>Show / Edit network location</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Florian Fittschen</xbar.author>
# <xbar.author.github>ffittschen</xbar.author.github>
# <xbar.desc>Displays the currently active network location and adds the ability to change it.</xbar.desc>
# <xbar.image>http://i.imgur.com/in3ZiPi.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Network/network-location.1h.sh</xbar.abouturl>

current_location=$(networksetup -getcurrentlocation)
network_locations=$(networksetup -listlocations) 

if [ ! -z "$1" ]; then
  networksetup -switchtolocation "$1"
fi

echo "📍 $current_location"
echo '---'
echo -n 'Current location: '
echo "$current_location"
echo '---'
echo 'Change to:'
while IFS= read -r location;
do
    if [ "${location}" != "${current_location}" ]
    then
        echo "${location} | bash=\"$0\" param1=\"$location\" terminal=false refresh=true"
    fi
done <<< "$network_locations"
