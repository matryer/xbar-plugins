#!/bin/bash

# <xbar.title>Nagios Prod</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Rob DeSanno</xbar.author>
# <xbar.author.github>rdesanno</xbar.author.github>
# <xbar.desc>Nagios status summary</xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

## set variables

URL="{set your nagios url here}"  			# ie nagios.example.com
NAME="{username}" 					# username
PASSWORD="{password}"					# password

TEMP_FILE="/tmp/nagios.out"
TAC="tac.cgi"
STATUS="status.cgi"
DOWN="?hostgroup=all&style=hostdetail&hoststatustypes=4&hostprops=42"
CRITICAL="?host=all&style=detail&servicestatustypes=16"
WARNING="?host=all&style=detail&servicestatustypes=4"
UNKNOWN="?host=all&style=detail&servicestatustypes=8"
OK="?host=all&style=detail&servicestatustypes=2"

curl -s -u "$NAME:$PASSWORD" "https://$URL/nagios/cgi-bin/$TAC" > $TEMP_FILE

down=$(grep "$DOWN" $TEMP_FILE | grep Down | cut -d\> -f3 | cut -d\< -f1)
critial=$(grep "$CRITICAL" $TEMP_FILE | grep Critical | cut -d\> -f3 | cut -d\< -f1)
warning=$(grep "$WARNING" $TEMP_FILE | grep Warning | cut -d\> -f3 | cut -d\< -f1)
unknown=$(grep "$UNKNOWN" $TEMP_FILE | grep Unknown | cut -d\> -f3 | cut -d\< -f1)
ok=$(grep "$OK" $TEMP_FILE | grep Ok | cut -d\> -f3 | cut -d\< -f1)

echo "$down | color=purple href=https://$URL/nagios/cgi-bin/$STATUS/$DOWN"
echo "$critial | color=red href=https://$URL/nagios/cgi-bin/$STATUS/$CRITICAL"
echo "$warning | color=brown href=https://$URL/nagios/cgi-bin/$STATUS/$WARNING"
echo "$unknown | color=orange href=https://$URL/nagios/cgi-bin/$STATUS/$UNKNOWN"
echo "$ok | color=green href=https://$URL/nagios/cgi-bin/$STATUS/$OK"
