#!/usr/bin/env bash

# <xbar.title>cal</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Buster Collings</xbar.author>
# <xbar.author.github>busterc</xbar.author.github>
# <xbar.desc>Shows a calendar of the current, previous and next year</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/busterc/bitbar-cal/master/screenshot.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.abouturl>https://github.com/busterc/bitbar-cal</xbar.abouturl>

year=$(date +%Y)
last_year=$((year-1))
next_year=$((year+1))

echo "$year"
echo "---"

echo "$last_year"
cal "$last_year" | while IFS= read -r i; do echo "--$i | trim=false font=courier"; done
(cal -h "$year" 2>/dev/null || cal "$year") | while IFS= read -r i; do echo "$i | trim=false font=courier"; done
cal "$next_year" | while IFS= read -r i; do echo "$i | trim=false font=courier"; done
