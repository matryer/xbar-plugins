#!/usr/bin/env bash
#
#  <xbar.title>Tilt</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Nick Sieger</xbar.author>
#  <xbar.author.github>nicksieger</xbar.author.github>
#  <xbar.desc>Control Tilt from the menubar instead of the command line.</xbar.desc>
#  <xbar.image>https://raw.githubusercontent.com/nicksieger/tilt-custom-extensions/main/xbar/tilt-xbar.png</xbar.image>
#  <xbar.abouturl>https://tilt.dev/</xbar.abouturl>
#
# This calls the tilt-xbar install script, which downloads itself and eventually
# overwrites this script inside the xbar plugins directory.

curl -sL https://raw.githubusercontent.com/nicksieger/tilt-custom-extensions/main/xbar/install.sh | bash
echo ""
