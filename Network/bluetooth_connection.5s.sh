#!/bin/zsh

# <xbar.title>Bluetooth Connection Indicator</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Simon Gray</xbar.author>
# <xbar.author.github>simongray</xbar.author.github>
# <xbar.desc>Indicate in the menu bar if a bluetooth device is connected.</xbar.desc>
# <xbar.image>https://github-production-user-asset-6210df.s3.amazonaws.com/5545555/239931189-6f427aec-b9c4-4ab0-b442-7a16050431cb.png</xbar.image>
# <xbar.dependencies>blueutil</xbar.dependencies>
# <xbar.abouturl>https://github.com/simongray</xbar.abouturl>

# Note: requires blueutil
# Install with `brew install blueutil`

export PATH='/usr/local/bin:/usr/bin:$PATH'

# Fixes missing Bluetooth connection status in macOS Big Sur
if [[ $(blueutil --connected) == *", paired"* ]]
then
	echo 🔹BT
else
	echo N/A
fi 
