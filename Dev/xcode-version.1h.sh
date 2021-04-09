#!/bin/bash
# <xbar.title>Xcode Version</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Florian Hirschmann</xbar.author>
# <xbar.author.github>hirschfl</xbar.author.github>
# <xbar.desc>Shows the Xcode version that is currently selected with xcode-select.</xbar.desc>
# <xbar.dependencies>xcodebuild</xbar.dependencies>

xcodebuild -version | head -1
