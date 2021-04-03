#!/bin/bash

# <xbar.title>Firewall</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Eddie A Tejeda</xbar.author>
# <xbar.author.github>eddietejeda</xbar.author.github>
# <xbar.desc>This plugin displays the status of the firewall.</xbar.desc>
# <xbar.image>http://i.imgur.com/MOBK8cK.png</xbar.image>
# <xbar.dependencies></xbar.dependencies>

enabled=$(/usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate);

if [[ $enabled == *"enabled"* ]]
then
  echo '☵'  
else
  echo '❌'
fi

echo '---'
echo "👁 $(/usr/libexec/ApplicationFirewall/socketfilterfw --getstealthmode)"
