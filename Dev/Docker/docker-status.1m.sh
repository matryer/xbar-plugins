#!/usr/bin/env bash
#
# <bitbar.title>Docker Status</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Manoj Mahalingam</bitbar.author>
# <bitbar.author.github>manojlds</bitbar.author.github>
# <bitbar.image>https://cloud.githubusercontent.com/assets/191378/12255368/1e671b32-b919-11e5-8166-6d975396f408.png</bitbar.image>
# <bitbar.desc>Displays the status of docker machines and running containers</bitbar.desc>
# <bitbar.dependencies>shell,docker</bitbar.dependencies>
#
# Docker status plugin
# by Manoj Mahalingam (@manojlds)
#
# Displays the status of docker machines and running containers

export PATH="/usr/local/bin:/usr/bin:$PATH"
echo "⚓️ | dropdown=false"
echo "---"

function containers() {
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
        *Up*) echo "$SYM $CONTAINER_NAME | color=green bash=\"$(command -v docker)\" param1=stop param2=$CONTAINER_ID terminal=false refresh=true";;
        *Exited*) echo "$SYM $CONTAINER_NAME | color=red bash=\"$(command -v docker)\" param1=start param2=$CONTAINER_ID terminal=false refresh=true";;
      esac
    done
  fi
}

if command -v docker-machine > /dev/null; then
    DOCKER_MACHINES="$(docker-machine ls -q)"
fi
if command -v dlite > /dev/null; then
    DLITE="$(command -v dlite)"
fi
if command -v docker > /dev/null; then
    DOCKER_NATIVE="$(command -v docker)"
fi

if test -z "$DOCKER_MACHINES" && test -z "$DLITE" && test -z "$DOCKER_NATIVE"; then
  echo "No docker machine, dlite or docker native found"
  exit 0
fi

if [ -n "$DOCKER_NATIVE" ]; then
  MACHINE="$($DOCKER_NATIVE -v)"
  CONTAINERS="$($DOCKER_NATIVE ps -a --format "{{.Names}} ({{.Image}})|{{.ID}}|{{.Status}}")"
  if [ -n "$CONTAINERS" ]; then
    echo "🔵  $MACHINE | bash=\"$DOCKER_NATIVE\" param1=stop terminal=false refresh=true"
    containers
  fi
  exit 0
fi

if [ -n "$DLITE" ]; then
  MACHINE="$($DLITE ip)"
  CONTAINERS="$(docker ps -a --format "{{.Names}} ({{.Image}})|{{.ID}}|{{.Status}}")"
  if [ -z "$CONTAINERS" ]; then
    echo "🔴  $MACHINE | bash=\"$DLITE\" param1=start terminal=false refresh=true"
  else
    echo "🔵  $MACHINE | bash=\"$DLITE\" param1=stop terminal=false refresh=true"
    containers
  fi
  exit 0
fi

if [ -n "$DOCKER_MACHINES" ]; then
  DM_EXEC=$(command -v docker-machine)
  echo "${DOCKER_MACHINES}" | while read -r machine; do
    STATUS=$($DM_EXEC status "$machine")
    if [ "$STATUS" = "Running" ]; then
      echo "🔵  $machine | bash=\"$DM_EXEC\" param1=stop param2=$machine terminal=false refresh=true"
      ENV=$($DM_EXEC env --shell sh "$machine")
      eval "$ENV"
      containers
    else
      echo "🔴  $machine | bash=\"$DM_EXEC\" param1=start param2=$machine terminal=false refresh=true"
    fi
    echo "---"
  done
fi

if [ -n "$CONTAINERS" ]; then
  echo "Docker VM Containers"
  containers
fi
