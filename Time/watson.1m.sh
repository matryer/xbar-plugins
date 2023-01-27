#!/bin/bash

# Watson Status
#
# by Antoine Corcy <contact@sbin.dk>
#
# <xbar.title>Watson Status</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Antoine Corcy</xbar.author>
# <xbar.author.github>toin0u</xbar.author.github>
# <xbar.desc>Shows Watson's status</xbar.desc>
# <xbar.dependencies>Watson</xbar.dependencies>
# <xbar.image>https://i.imgur.com/3OuXrWn.png</xbar.image>
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
tags=$(echo "$status" | awk -F "[][]" '{printf "Tags: %s\n", $2}')

# main
echo "$project"
echo "---"
echo "${started/(/} | color=green"
echo "$tags"
