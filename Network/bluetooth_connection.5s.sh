#!/bin/zsh

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
