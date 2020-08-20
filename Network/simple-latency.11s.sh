#! /bin/zsh
# <bitbar.title>Simple-Latency</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jerome</bitbar.author>
# <bitbar.author.github>fine-fiddle</bitbar.author.github>
# <bitbar.desc>Display a color categorized running average of your pingtime to an endpoint, plus any dropped packets.</bitbar.desc>
# <bitbar.image>https://imgur.com/oGuCKUd</bitbar.image>
# <bitbar.dependencies>zsh</bitbar.dependencies>


GREEN='\033[1;32m';
YELLOW='\033[1;33m';
BLUE='\033[1;34m';
PURPLE='\033[1;35m';
RED='\033[1;31m';
NC='\033[0m';

# NOTE- this must be less than the refresh time in the filename of this plugin. 
PINGSPERITERATION=10;
PINGDESTINATION=8.8.8.8;

PINGOUT=$(ping -c ${PINGSPERITERATION} ${PINGDESTINATION} | tail -n 2);
LATENCY=$(echo "${PINGOUT}" | tail -n 1 | sed 's/.*= //' | awk 'BEGIN { FS="/" } ; { print $2 }');
DROPPED=$(echo "${PINGOUT}" | head -n 1 | awk '{ print $7 }');

if [[ $LATENCY -lt 15 ]]; then PREFIX=$GREEN; fi;
if [[ $LATENCY -ge 15 ]]; then PREFIX=$BLUE; fi;
if [[ $LATENCY -ge 40 ]]; then PREFIX=$PURPLE; fi;
if [[ $LATENCY -ge 70 ]]; then PREFIX=$RED; fi;

if [[ $DROPPED != "0.0%" ]]; then 
    PREFIX=$RED; 
    echo "${PREFIX}$LATENCY•$DROPPED${NC}";
    return 0;
fi;

echo "${PREFIX}$LATENCY${NC}";