#!/bin/bash

# XcodeVersion
# Xcode version plugin
#
# by Florian Hirschmann
#
# Shows the Xcode version that is currently selected with xcode-select. 
# This is especially useful if you have multiple version of Xcode installed.
#
# Depends on Xcode / Apple Developer Tools
#
xcodebuild -version | head -1

