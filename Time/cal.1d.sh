#!/usr/bin/env bash

# <bitbar.title>cal</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Buster Collings</bitbar.author>
# <bitbar.author.github>busterc</bitbar.author.github>
# <bitbar.desc>Shows a calendar of the current, previous and next year</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/busterc/bitbar-cal/master/screenshot.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/busterc/bitbar-cal</bitbar.abouturl>

year=$(date +%Y)
last_year=$((year-1))
next_year=$((year+1))

echo "$year"
echo "---"

echo "$last_year"
cal "$last_year" | while IFS= read -r i; do echo "--. $i | font=courier"; done
cal "$year" | while IFS= read -r i; do echo ". $i | font=courier"; done
cal "$next_year" | while IFS= read -r i; do echo ". $i | font=courier"; done
