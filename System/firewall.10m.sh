#!/bin/bash

# <bitbar.title>Firewall</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Eddie A Tejeda</bitbar.author>
# <bitbar.author.github>eddietejeda</bitbar.author.github>
# <bitbar.desc>This plugin displays the status of the firewall.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/MOBK8cK.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>

enabled=$(/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate);

if [[ $enabled == *"enabled"* ]]
then
  echo '☵'  
else
  echo '❌'
fi

echo '---'
echo "👁 $(/usr/libexec/ApplicationFirewall/socketfilterfw --getstealthmode)"
