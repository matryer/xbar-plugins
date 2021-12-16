#!/bin/bash
# <xbar.title>PIA VPN Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Parvez</xbar.author>
# <xbar.author.github>parvez</xbar.author.github>
# <xbar.desc>Displays PIA VPN connection status</xbar.desc>
# <xbar.image>http://i.imgur.com/YqBwZed.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/parvez/bitbar-plugins</xbar.abouturl>
#
# by Parvez
pia1=$(curl --silent https://www.privateinternetaccess.com/pages/whats-my-ip/)
pia2=$(echo "$pia1" | tr -d "\n" | /usr/local/bin/gsed -r "s/<script([^<]|<[^\/]|<\/[^s]|<\/s[^c])*<\/script>|<style([^<]|<[^\/]|<\/[^s]|<\/s[^t])*<\/style>//g" | grep -o '<div class="ipbox-footer">.*<div class="ipbox-map">' | /usr/local/bin/gsed -r "s/<[^>]*>//g" | /usr/local/bin/gsed -r "s/:\s+/: /g" | /usr/local/bin/gsed -r "s/\s\s+/\r\n/g")

if [[ $pia1 == *"You are protected by PIA"* ]]
then
  echo "✅"
else
  echo "🚫"
fi

echo "---"
echo "$pia2"
