#!/bin/bash

# <xbar.title>Wallpaper refresh</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Anmol Arora</xbar.author>
# <xbar.author.github>anmolarora</xbar.author.github>
# <xbar.desc>A new HD wallpaper every hour.</xbar.desc>

echo 'W'
echo ---

# Download image from server
curl "$(curl https://powerful-inlet-26346.herokuapp.com/image_url)" > "/tmp/wallpaper_refresh_image.png"
# Set image as wallpaper
SCRIPT_TEXT='tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper_refresh_image.png"'
osascript -e "${SCRIPT_TEXT}"
