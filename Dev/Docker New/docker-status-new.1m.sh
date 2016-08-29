#!/usr/bin/env bash
#
# <bitbar.title>Docker Status (For the New Docker for Mac)</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>James Baker</bitbar.author>
# <bitbar.author.github>rael9</bitbar.author.github>
# <bitbar.image>http://www.jmblog.org/stuff/docker-script.png</bitbar.image>
# <bitbar.desc>Displays the status of docker machines running in the new Docker for Mac</bitbar.desc>
# <bitbar.dependencies>shell,docker</bitbar.dependencies>
#
# Docker status plugin for the new Docker for Mac
# by Jim Baker (@rael9) based on Docker status plugin by Manoj Mahalingam (@manojlds)
#
# Displays the status of your docker machines

export PATH="/usr/local/bin:/usr/bin:$PATH"
echo "⚓️ | dropdown=false"
echo "---"

CONTAINERS="$(docker ps -a --format "{{.Names}} ({{.Image}})|{{.ID}}|{{.Status}}")"
if [ -z "$CONTAINERS" ]; then
  echo "No running containers"
else
  LAST_CONTAINER=$(echo "$CONTAINERS" | tail -n1 )
  echo "${CONTAINERS}" | while read -r CONTAINER; do
    CONTAINER_NAME=$(echo "$CONTAINER" | awk -F"|" '{print $1}')
    CONTAINER_ID=$(echo "$CONTAINER" | awk -F"|" '{print $2}')
    CONTAINER_STATE=$(echo "$CONTAINER" | awk -F"|" '{print $3}')
    SYM="├ 💻 "
    if [ "$CONTAINER" = "$LAST_CONTAINER" ]; then SYM="└ 💻 "; fi
    case "$CONTAINER_STATE" in
      *Up*) echo "$SYM $CONTAINER_NAME | color=green bash=$(which docker) param1=stop param2=$CONTAINER_ID terminal=false refresh=true";;
      *Exited*) echo "$SYM $CONTAINER_NAME | color=red bash=$(which docker) param1=start param2=$CONTAINER_ID terminal=false refresh=true";;
    esac
  done
fi
