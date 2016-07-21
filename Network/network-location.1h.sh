#!/bin/bash
# <bitbar.title>Show / Edit network location</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Florian Fittschen</bitbar.author>
# <bitbar.author.github>ffittschen</bitbar.author.github>
# <bitbar.desc>Displays the currently active network location and adds the ability to change it.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/in3ZiPi.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/matryer/bitbar-plugins/blob/master/Network/network-location.1h.sh</bitbar.abouturl>

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
