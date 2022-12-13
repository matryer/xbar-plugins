#!/bin/bash
# <xbar.title>Irish Rail Trains</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Daniel Burke</xbar.author>
# <xbar.author.github>dan.burke</xbar.author.github>
# <xbar.desc>Retrieves next three trains at a particular station from Irish Rail</xbar.desc>
# <xbar.image>http://i.imgur.com/PRa5B1m.png</xbar.image>
# <xbar.dependencies>bash,jq</xbar.dependencies>
# Depends on jq - https://stedolan.github.io/jq/

# Replace the STATIONID with the appropriate ID
STATIONID='PERSE'
# Replace the DIRECTION with the appropriate DIRECTION
DIRECTION='Southbound'
json=$(curl -s "https://dublin-trains.herokuapp.com/api/stations/$STATIONID/trains" |  /usr/local/bin/jq -r '[ .[] | select(.direction ==  "'$DIRECTION'")| select(.train_type ==  "DART")]')
echo "$json" | /usr/local/bin/jq -r 'map(select(.scheduled | contains ("--") | not) )| .[0] | "\(.last_update) - \(.due_in)"'
echo '---'
echo "$json" | /usr/local/bin/jq -r 'map(select(.scheduled | contains ("--") | not) )| .[] | "\(.destination): \(.last_update) - \(.actual)"'
