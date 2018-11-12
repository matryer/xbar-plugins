#!/bin/bash
# <bitbar.title>Recent Big Earthquake</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Aaron Edell</bitbar.author>
# <bitbar.author.github>aaronedell</bitbar.author.github>
# <bitbar.desc>Displays the most significant earthquakes in the last 30 days from http://earthquake.usgs.gov </bitbar.desc>
# <bitbar.image>http://i.imgur.com/lF8Qdpk.png</bitbar.image>
# <bitbar.dependencies>Bash GNU AWK</bitbar.dependencies>
# <bitbar.abouturl>http://earthquake.usgs.gov</bitbar.abouturl>



echo '🌎'
echo '---'

echo ; curl -s https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.csv | tail -n +2 | awk -F"," '{print $5" " $14$15}'
echo '---'
echo 'Data from http://earthquake.usgs.gov | color=green href=http://earthquake.usgs.gov'



