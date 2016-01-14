#!/bin/bash
# <bitbar.title>Recent Big Earthquake</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Aaron Edell</bitbar.author>
# <bitbar.author.github>aaronedell</bitbar.author.github>
# <bitbar.desc>Displays the most significant earthquakes in the last 30 days from http://earthquake.usgs.gov </bitbar.desc>
# <bitbar.image>https://d30y9cdsu7xlg0.cloudfront.net/png/8681-200.png</bitbar.image>
# <bitbar.dependencies>Bash gnu AWK</bitbar.dependencies>
# <bitbar.abouturl>http://earthquake.usgs.gov</bitbar.abouturl>



echo 'EarthQuakes'
echo '---'
echo 'Data from http://earthquake.usgs.gov | color=green href=http://earthquake.usgs.gov'
echo '---'
echo ; curl -s http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.csv | tail -n +2 | awk -F"," '{print $5" " $14$15}'




