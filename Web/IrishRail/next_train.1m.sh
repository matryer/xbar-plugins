#! /bin/bash
# <bitbar.title>Irish Rail Trains</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Daniel Burke</bitbar.author>
# <bitbar.author.github>dan.burke</bitbar.author.github>
# <bitbar.desc>Short description of what your plugin does.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/PRa5B1m.png</bitbar.image>
# <bitbar.dependencies>bash,jq</bitbar.dependencies>
# Depends on jq - https://stedolan.github.io/jq/

# Replace the STATIONID with the appropriate ID
STATIONID='PERSE'
# Replace the DIRECTION with the appropriate DIRECTION
DIRECTION='Southbound'
json=$(curl -s https://dublin-trains.herokuapp.com/api/stations/$STATIONID/trains |  /usr/local/bin/jq -r '.["'$DIRECTION'"]')
echo "$json" | /usr/local/bin/jq -r 'map(select(.scheduled | contains ("--") | not) )| .[0].last_update'
echo '---'
echo "$json" | /usr/local/bin/jq -r 'map(select(.scheduled | contains ("--") | not) )| .[] | "\(.destination): \(.last_update)"'
