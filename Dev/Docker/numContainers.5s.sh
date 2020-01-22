#!/bin/bash

# <bitbar.title>NumContainers</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>cghamburg</bitbar.author>
# <bitbar.author.github>cghamburg</bitbar.author.github>
# <bitbar.desc>Print number of running Docker containers with whale unicode char</bitbar.desc>
# <bitbar.dependencies>docker</bitbar.dependencies>

CONTAINERS=$(/usr/local/bin/docker ps --format '{{.Names}}' | sort)
NUM_CONTAINERS=0
if [ -n "$CONTAINERS" ]
then
	NUM_CONTAINERS=$(echo "${CONTAINERS}" | wc -l | tr -d '[:space:]')
fi
echo "$(printf "🐳 %.0f \n" "${NUM_CONTAINERS}") | size=13"
echo "---"
echo "${CONTAINERS}"
