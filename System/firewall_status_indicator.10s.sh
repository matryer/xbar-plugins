#!/bin/bash

# <bitbar.title>Firewall Status Indicator</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Miles Wolbe</bitbar.author>
# <bitbar.author.github>tinyapps</bitbar.author.github>
# <bitbar.desc>Display status of the built-in Application Firewall</bitbar.desc>
# <bitbar.image>https://tinyapps.org/screenshots/bitbar_firewall_status_indicator.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://tinyapps.org/blog/mac/201709100715_mac_firewall_status.html/</bitbar.abouturl>

state=$(defaults read "/Library/Preferences/com.apple.alf" globalstate)

if [ "$state" -eq 2 ]; then
  echo "🔒"
elif [ "$state" -eq 1 ]; then
  echo "❗️"
else
  echo "‼️"
fi

echo "---"
echo "Open Firewall preference pane| href='x-apple.systempreferences:com.apple.preference.security?Firewall'"
