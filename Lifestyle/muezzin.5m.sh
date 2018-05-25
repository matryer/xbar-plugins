#!/bin/bash
#
# <bitbar.title>Muezzin</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Mehmet Akif Tütüncü</bitbar.author>
# <bitbar.author.github>mehmetakiftutuncu</bitbar.author.github>
# <bitbar.image>https://github.com/mehmetakiftutuncu/Muezzin-BitBar/raw/master/Screenshot.png</bitbar.image>
# <bitbar.desc>This is a BitBar plugin for showing Islamic prayer times in your menu bar. See https://github.com/mehmetakiftutuncu/Muezzin-BitBar#configuration for configuration.</bitbar.desc>
# <bitbar.dependencies>shell,httpie,jq</bitbar.dependencies>
#
# Muezzin by Mehmet Akif Tütüncü
#
# This is a BitBar plugin for showing Islamic prayer times in your menu bar.
# See https://github.com/mehmetakiftutuncu/Muezzin-BitBar#configuration for configuration.

export PATH="/usr/local/bin:$PATH"

COUNTRY_ID="2"
CITY_ID="539"
DISTRICT_ID="9541"

# Needs to be absolute path and make sure you have write permissions there.
DATA_FILE="/tmp/.muezzin.tsv"

url="https://muezzin.herokuapp.com/prayerTimes/country/$COUNTRY_ID/city/$CITY_ID/district/$DISTRICT_ID"
today=$(date +%Y-%m-%d)
now=$(date +%H:%M)

http --ignore-stdin $url | jq -r -c ".prayerTimes.\"$today\" | to_entries[] | [.key, .value] | @tsv" > $DATA_FILE

nextPrayerTimeName=$(awk '$2>"'"$now"'" {print toupper(substr($1,1,1))tolower(substr($1,2))}' $DATA_FILE | head -n 1)
nextPrayerTime=$(awk '$2>"'"$now"'" {print $2}' $DATA_FILE | head -n 1)

echo "🕌 $nextPrayerTimeName ⏰ $nextPrayerTime"
echo "---"
awk '$1!="qibla"{print toupper(substr($1,1,1))tolower(substr($1,2))": "$2}' $DATA_FILE