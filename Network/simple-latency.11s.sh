#! /bin/zsh
# <xbar.title>Simple-Latency</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jerome</xbar.author>
# <xbar.author.github>fine-fiddle</xbar.author.github>
# <xbar.desc>Display a color categorized running average of your pingtime to an endpoint, plus any dropped packets.</xbar.desc>
# <xbar.image>https://i.imgur.com/oGuCKUd.png</xbar.image>
# <xbar.dependencies>zsh</xbar.dependencies>


GREEN='\033[1;32m';
YELLOW='\033[1;33m';
BLUE='\033[1;34m';
PURPLE='\033[1;35m';
RED='\033[1;31m';
NC='\033[0m';

# NOTE- this must be less than the refresh time in the filename of this plugin. 
PINGSPERITERATION=10;
PINGDESTINATIONS=(8.8.8.8);
PINGINTERVAL=0.1;

for PINGDESTINATION in "${PINGDESTINATIONS[@]}"; do
  PINGOUT=$(ping -i ${PINGINTERVAL} -c ${PINGSPERITERATION} ${PINGDESTINATION} | tail -n 2);
  LATENCY=$(echo "${PINGOUT}" | tail -n 1 | sed 's/.*= //' | awk 'BEGIN { FS="/" } ; { print $2 }');
  DROPPED=$(echo "${PINGOUT}" | head -n 1 | awk '{ print $7 }');

  if [[ $LATENCY -lt 15 ]]; then PREFIX=$GREEN; fi;
  if [[ $LATENCY -ge 15 ]]; then PREFIX=$BLUE; fi;
  if [[ $LATENCY -ge 40 ]]; then PREFIX=$PURPLE; fi;
  if [[ $LATENCY -ge 70 ]]; then PREFIX=$RED; fi;

  if [[ $DROPPED != "0.0%" ]]; then
      PREFIX=$RED;
      echo "${PINGDESTINATION/.[a-z]*/} ${PREFIX}$LATENCY•$DROPPED${NC}";
      continue;
  fi;

  echo "${PINGDESTINATION/.[a-z]*/} ${PREFIX}$LATENCY${NC}";
done
