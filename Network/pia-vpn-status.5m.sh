#!/bin/bash
# <bitbar.title>PIA VPN Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Parvez</bitbar.author>
# <bitbar.author.github>parvez</bitbar.author.github>
# <bitbar.desc>Displays PIA VPN connection status</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/parvez/bitbar-plugins</bitbar.abouturl>
#
# by Parvez
pia=$(curl --silent https://www.privateinternetaccess.com/pages/whats-my-ip/| tr -d "\n" | grep -o '<div class="topheader">.* curl' | sed -e 's/<[^>]*>//g' | /usr/local/bin/gsed -e 's/  \+/\n/g');
if [[ $pia == *"You are protected by PIA"* ]]
then
  echo "✅";
else
  echo "🚫";
fi
echo '---';
echo "$pia";
