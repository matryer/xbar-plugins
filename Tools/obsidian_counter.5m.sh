#!/bin/bash

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Obsidian Counter</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Daniel Mathiot</xbar.author>
#  <xbar.author.github>danymat</xbar.author.github>
#  <xbar.desc>Display the number of notes in the specified location</xbar.desc>
#  <xbar.image>https://raw.githubusercontent.com/danymat/personal-xbar-plugins/main/images/obsidian_2021_03_30.png</xbar.image>
#  <xbar.var>string(vault="/Users/danielmathiot/.config/skhd/"): The location of your current vault.</xbar.var>
#  <xbar.dependencies></xbar.dependencies>
#  <xbar.abouturl></xbar.abouturl>

number_notes=$(ls -p "$vault" | grep -v / | wc -l)
vault_name_uri=$(echo $vault |  sed -e 's:.*/::' -e 's/ /%20/g')

echo $number_notes Notes
echo "---"
echo "Vault folder: $vault"
echo "Open Obsidian vault | href=obsidian://open?vault=$vault_name_uri"
