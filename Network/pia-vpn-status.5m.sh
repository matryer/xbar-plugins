#!/bin/bash
# <bitbar.title>PIA VPN Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Parvez</bitbar.author>
# <bitbar.author.github>parvez</bitbar.author.github>
# <bitbar.desc>Displays PIA VPN connection status</bitbar.desc>
# <bitbar.image>http://i.imgur.com/YqBwZed.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/parvez/bitbar-plugins</bitbar.abouturl>
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
