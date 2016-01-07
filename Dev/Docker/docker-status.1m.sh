#!/usr/bin/env sh
#
# Docker plugin
# by Manoj Mahalingam (@manojlds)
#
# Displays the status of docker machines and running containers


export PATH="/usr/local/bin:/usr/bin:$PATH"
echo "⚓️"
echo "---"

DOCKER_MACHINES="$(docker-machine ls -q)"
if [ -z "$DOCKER_MACHINES" ]; then
  echo "No docker machine found"
  exit 0
fi

echo "${DOCKER_MACHINES}" | while read -r machine; do
  STATUS=$(docker-machine status "$machine")
  if [ "$STATUS" = "Running" ]; then
    echo "$machine | color=green"
    ENV=$(docker-machine env --shell sh "$machine")
    eval "$ENV"
    CONTAINERS="$(docker ps --format "{{.Names}} ({{.Image}})")"
    if [ -z "$CONTAINERS" ]; then
      echo "No running containers"
    else
      echo "${CONTAINERS}" | while read -r container; do
        echo "🆙 $container | color=black"
      done
    fi
  else
    echo "$machine (not running)"
  fi
  echo "---"
done
