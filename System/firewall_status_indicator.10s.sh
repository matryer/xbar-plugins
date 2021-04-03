#!/bin/bash

# <xbar.title>Firewall Status Indicator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Miles Wolbe</xbar.author>
# <xbar.author.github>tinyapps</xbar.author.github>
# <xbar.desc>Display status of the built-in Application Firewall</xbar.desc>
# <xbar.image>https://tinyapps.org/screenshots/bitbar_firewall_status_indicator.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://tinyapps.org/blog/mac/201709100715_mac_firewall_status.html/</xbar.abouturl>

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
