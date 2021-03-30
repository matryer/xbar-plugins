#!/bin/bash

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Personal Configs</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Daniel Mathiot</xbar.author>
#  <xbar.author.github>danymat</xbar.author.github>
#  <xbar.desc>Display the personal user configs such as hyper keys in skhd</xbar.desc>
#  <xbar.image>https://raw.githubusercontent.com/danymat/personal-xbar-plugins/53ae1fde8337216174ebcad4ba42870d5bcfbb1a/images/config_2021_03_30.png</xbar.image>
#  <xbar.var>string(SKHD_LOCATION="/Users/danielmathiot/.config/skhd/"): The location of your skhd config file.</xbar.var>
#  <xbar.var>string(ICON_NAVBAR="⛏"): Navbar icon.</xbar.var>
#  <xbar.var>string(ICON_COMMANDS="🛠"): Icon for the skhd commands .</xbar.var>
#  <xbar.dependencies></xbar.dependencies>
#  <xbar.abouturl></xbar.abouturl>

## README

### SKHD
# In order for the hyper key to be recognized, you need to have in your skhd file:
# example: # HYPER A: Do some stuff
#          hyper - a : yabai -m ...

##
echo $ICON_NAVBAR
echo "---"
echo "skhd"
echo "-- Open skhd folder| shell=open | param1=$SKHD_LOCATION"
cat $SKHD_LOCATION/skhdrc | grep "# HYPER\b" | sed -e 's/# HYPER/--'$ICON_COMMANDS'/g'| xargs -L1 echo
