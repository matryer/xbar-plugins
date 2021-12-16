#!/bin/bash
# <xbar.title>OpenUI5 Latest Version Display</xbar.title>
# <xbar.author>DJ Adams</xbar.author>
# <xbar.author.github>qmacro</xbar.author.github>
# <xbar.image>https://i.imgur.com/5GVxySB.png</xbar.image>
# <xbar.version>1.0</xbar.version>

# OpenUI5 Latest Version Display
# BitBar plugin
#
# by DJ Adams

URI="https://openui5.hana.ondemand.com/resources/sap-ui-version.json"

VER=$(curl -s --compressed $URI | grep '"version"' | head -1 | sed -E 's/^.+([0-9]+\.[0-9]+\.[0-9]+).+$/\1/')

echo "$VER"
