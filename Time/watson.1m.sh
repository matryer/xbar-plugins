#!/bin/bash

# Watson Status
#
# by Antoine Corcy <contact@sbin.dk>
#
# <bitbar.title>Watson Status</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Antoine Corcy</bitbar.author>
# <bitbar.author.github>toin0u</bitbar.author.github>
# <bitbar.desc>Shows Watson's status</bitbar.desc>
# <bitbar.dependencies>Watson</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/3OuXrWn.png</bitbar.image>
#
# Dependencies:
#   watson (http://tailordev.github.io/Watson/)

# get the status text
status=$(/usr/local/bin/watson status)

# show watson
if [[ "$status" == "No project started" ]]
then
    echo "⏱ Watson"
    exit 1
fi

# get the project name
project=$(echo "$status" | awk '{printf "⏱ %s", $2}')

# get the started time and uppercase the sentence
started=$(echo "$status" | grep -E -o 'started (.*) \(')
started="$(tr '[:lower:]' '[:upper:]' <<< "${started:0:1}")${started:1}"

# get the tags
tags=$(echo "$status" | awk '{printf "Tags: %s\n", $3}')

# main
echo "$project"
echo "---"
echo "${started/(/} | color=green"
echo "$tags"
