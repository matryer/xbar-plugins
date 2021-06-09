#!/usr/bin/env bash

# <bitbar.title>YNAB Bitbar</bitbar.title>
# <bitbar.version>1.0.0</bitbar.version>
# <bitbar.author>Noah Phillips</bitbar.author>
# <bitbar.author.github>noahsphillips</bitbar.author.github>
# <bitbar.desc>Show your budgeted/unbudgeted balances from YNAB</bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.dependencies>node</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/noahsphillips/ynab-bitbar</bitbar.abouturl>

INSTALLER_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
INSTALLER_PATH="$INSTALLER_DIR/$(basename $0)"

# Go to the BitBar plugins directory
cd "$(defaults read com.matryer.BitBar pluginsDirectory)"

# If already installed, check for version updates
if [ -d "ynab-bitbar" ]; then
	cd ynab-bitbar
	echo "Updating ynab-bitbar..."
	git pull origin master --quiet
	echo "Updated successfully."
# If not installed, clone the repository
else
	echo "Downloading ynab-bitbar..."
	git clone https://github.com/noahsphillips/ynab-bitbar --quiet
	echo "Downloaded successfully."
	cd ynab-bitbar
fi

# Install node dependencies
echo "Installing npm dependencies..."
npm install &>/dev/null
echo "Dependencies installed."

# Create the symlink if it doesn't exist
cd ..
if ! [ -L "./ynab-bitbar.15m.js" ]; then
	echo "Linking..."
	ln -s ynab-bitbar/ynab-bitbar.15m.js
	echo "Linked."
fi

# Refresh the plugin
echo "Refreshing plugin..."
open "bitbar://refreshPlugin?name=ynab-bitbar.15m.js"
echo "Plugin refreshed."

# Delete this script!
echo "Deleting installer..."
rm -- "$INSTALLER_PATH"
echo "Installer deleted."