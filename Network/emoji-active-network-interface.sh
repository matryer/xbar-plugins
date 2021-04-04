#!/bin/bash

# <xbar.title>Emoji Active Network Interface Indicator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Zachary O. Toups</xbar.author>
# <xbar.author.github>toupsz</xbar.author.github>
# <xbar.desc>Displays an emoji to indicate if the active network interface is wifi or another connection.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/toupsz/emoji-active-network-interface-indicator</xbar.abouturl>

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
