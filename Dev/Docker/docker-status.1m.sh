#!/usr/bin/env sh
#
# <bitbar.title>Docker Status</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Manoj Mahalingam</bitbar.author>
# <bitbar.author.github>manojlds</bitbar.author.github>
# <bitbar.desc>Displays the status of docker machines and running containers</bitbar.desc>
# <bitbar.dependencies>shell,docker,docker-machine</bitbar.dependencies>
#
# Docker status plugin
# by Manoj Mahalingam (@manojlds)
#
# Displays the status of docker machines and running containers

export PATH="/usr/local/bin:/usr/bin:$PATH"
echo "⚓️ | dropdown=false"
echo "---"

DOCKER_MACHINES="$(docker-machine ls -q)"
if [ -z "$DOCKER_MACHINES" ]; then
  echo "No docker machine found"
  exit 0
fi

echo "${DOCKER_MACHINES}" | while read -r machine; do
  STATUS=$(docker-machine status "$machine")
  if [ "$STATUS" = "Running" ]; then
    echo "$machine | color=green bash=$(which docker-machine) param1=stop param2=$machine terminal=false refresh=true"
    ENV=$(docker-machine env --shell sh "$machine")
    eval "$ENV"
    CONTAINERS="$(docker ps --format "{{.Names}} ({{.Image}})|{{.ID}}")"
    if [ -z "$CONTAINERS" ]; then
      echo "No running containers"
    else
      LAST_CONTAINER=$(echo "$CONTAINERS" | tail -n1 )
      echo "${CONTAINERS}" | while read -r CONTAINER; do
        CONTAINER_ID=$(echo "$CONTAINER" | sed 's/.*|//')
        CONTAINER_NAME=$(echo "$CONTAINER" | sed 's/|.*//')
        SYM="├"
        if [ "$CONTAINER" = "$LAST_CONTAINER" ]; then SYM="└"; fi
        echo "$SYM $CONTAINER_NAME | color=green bash=$(which docker) param1=stop param2=$CONTAINER_ID terminal=false refresh=true"
      done
    fi
  else
    echo "$machine (not running) | color=red bash=$(which docker-machine) param1=start param2=$machine terminal=false refresh=true"
  fi
  echo "---"
done
