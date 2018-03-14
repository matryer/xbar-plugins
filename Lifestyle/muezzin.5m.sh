#!/bin/bash
#
# <bitbar.title>Muezzin</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mehmet Akif Tütüncü</bitbar.author>
# <bitbar.author.github>mehmetakiftutuncu</bitbar.author.github>
# <bitbar.image>https://github.com/mehmetakiftutuncu/Muezzin-BitBar/raw/master/Screenshot.png</bitbar.image>
# <bitbar.desc>This is a BitBar plugin for showing Islamic prayer times in your menu bar.</bitbar.desc>
# <bitbar.dependencies>shell,httpie,jq</bitbar.dependencies>
#
# Muezzin by Mehmet Akif Tütüncü
#
# This is a BitBar plugin for showing Islamic prayer times in your menu bar.

export PATH="/usr/local/bin:$PATH"

COUNTRY_ID="2"
CITY_ID="539"
DISTRICT_ID="9541"

URL="https://muezzin.herokuapp.com/prayerTimes/country/$COUNTRY_ID/city/$CITY_ID/district/$DISTRICT_ID"

TODAY=$(date +%Y-%m-%d)
NOW=$(date +%H:%M)

DATA=$(http --ignore-stdin $URL | jq -r ".prayerTimes.\"$TODAY\" | to_entries[] | [.key, .value] | @tsv")
TITLE=$(echo "$DATA" | awk '$2>"'$NOW'"{print toupper(substr($1,0,1))tolower(substr($1,2))" 🕌 "$2}' | head -n 1)

echo "$TITLE"
echo "---"
echo "$DATA" | awk '$1!="qibla"{print toupper(substr($1,0,1))tolower(substr($1,2))": "$2}'
