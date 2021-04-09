#!/bin/bash

# <xbar.title>NumContainers</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>cghamburg</xbar.author>
# <xbar.author.github>cghamburg</xbar.author.github>
# <xbar.desc>Print number of running Docker containers with whale unicode char</xbar.desc>
# <xbar.dependencies>docker</xbar.dependencies>

CONTAINERS=$(/usr/local/bin/docker ps --format '{{.Names}}' | sort)
NUM_CONTAINERS=0
if [ -n "$CONTAINERS" ]
then
	NUM_CONTAINERS=$(echo "${CONTAINERS}" | wc -l | tr -d '[:space:]')
fi
echo "$(printf "🐳 %.0f \n" "${NUM_CONTAINERS}") | size=13"
echo "---"
echo "${CONTAINERS}"
