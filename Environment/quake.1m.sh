#!/bin/bash
# <xbar.title>Recent Big Earthquake</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Aaron Edell</xbar.author>
# <xbar.author.github>aaronedell</xbar.author.github>
# <xbar.desc>Displays the most significant earthquakes in the last 30 days from http://earthquake.usgs.gov </xbar.desc>
# <xbar.image>http://i.imgur.com/lF8Qdpk.png</xbar.image>
# <xbar.dependencies>Bash GNU AWK</xbar.dependencies>
# <xbar.abouturl>http://earthquake.usgs.gov</xbar.abouturl>



echo '🌎'
echo '---'

echo ; curl -s https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/significant_month.csv | tail -n +2 | awk -F"," '{print $5" " $14$15}'
echo '---'
echo 'Data from http://earthquake.usgs.gov | color=green href=http://earthquake.usgs.gov'



