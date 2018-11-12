#!/bin/bash

# <bitbar.title>Wallpaper refresh</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Anmol Arora</bitbar.author>
# <bitbar.author.github>anmolarora</bitbar.author.github>
# <bitbar.desc>A new HD wallpaper every hour.</bitbar.desc>

echo 'W'
echo ---

# Download image from server
curl "$(curl https://powerful-inlet-26346.herokuapp.com/image_url)" > "/tmp/wallpaper_refresh_image.png"
# Set image as wallpaper
SCRIPT_TEXT='tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper_refresh_image.png"'
osascript -e "${SCRIPT_TEXT}"
