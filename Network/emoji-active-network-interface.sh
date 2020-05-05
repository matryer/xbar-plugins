#!/bin/bash

# <bitbar.title>Emoji Active Network Interface Indicator</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Zachary O. Toups</bitbar.author>
# <bitbar.author.github>toupsz</bitbar.author.github>
# <bitbar.desc>Displays an emoji to indicate if the active network interface is wifi or another connection.</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/toupsz/emoji-active-network-interface-indicator</bitbar.abouturl>

# Text above --- displays an antenna emoji if the active network interface name contains "Wi-Fi", shows a plug otherwise.

# Get the active network metadata
ActiveNetwork=$(route get default | grep interface | awk '{print $2}')

# Get the name of the active network from the metadata
ActiveNetworkName=$(networksetup -listallhardwareports | grep -B 1 "$ActiveNetwork" | awk '/Hardware Port/{ print }'|cut -d " " -f3-)

# Select an emoji based on the name
if [[ $ActiveNetworkName = *"Wi-Fi"* ]]; then
	echo "📡"
else
	echo "🔌"
fi
