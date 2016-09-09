#!/bin/bash
# <bitbar.title>OpenUI5 Latest Version Display</bitbar.title>
# <bitbar.author>DJ Adams</bitbar.author>
# <bitbar.author.github>qmacro</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/5GVxySB.png</bitbar.image>
# <bitbar.version>1.0</bitbar.version>

# OpenUI5 Latest Version Display
# BitBar plugin
#
# by DJ Adams

URI="https://openui5.hana.ondemand.com/resources/sap-ui-version.json"

VER=$(curl -s --compressed $URI | grep '"version"' | head -1 | sed -E 's/^.+([0-9]+\.[0-9]+\.[0-9]+).+$/\1/')

echo "$VER"
