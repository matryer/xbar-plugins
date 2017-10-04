#!/bin/bash

# <bitbar.title>Drone Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Christoph Schlosser</bitbar.author>
# <bitbar.author.github>christophschlosser</bitbar.author.github>
# <bitbar.desc>Checks the status of the builds from Drone CI</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/christophschlosser/bitbar-plugins/drone-status/Dev/Drone/drone-status.png</bitbar.image>
# <bitbar.dependencies>jq,bash,curl,awk</bitbar.dependencies>

#################
# User Settings #
#################

# Needed for jq, curl, awk. If you install jq somewhere else you have to add it here as well
export PATH="/usr/local/bin:/usr/bin:$PATH"

# Get jq from here: https://stedolan.github.io/jq/

# URL to Drone Web Interface
DRONE_URL="https://drone.example.com"

# The Account token from Webinterface -> Account -> Show Token
TOKEN="some.long.token"

##################
# Implementation #
##################
# Get repos for current user
json=$(curl --silent -sb -H "Accept: application/json" -H "Authorization: $TOKEN" -X GET "$DRONE_URL/api/user/repos")

# Parse active repo names from JSON
repos=($(echo "$json" | jq '.[] | select(.last_build>0) | {name: .full_name, active: .last_build}' | grep name | awk '{ print $2}'))
# Parse last build number from JSON
builds=($(echo "$json" | jq '.[] | select(.last_build>0) | {name: .full_name, active: .last_build}' | grep active | awk '{ print $2}'))

success=0
failure=0
running=0

output=

for i in "${!repos[@]}"; do
    repo=${repos[$i]//[,\"]/}
    build=${builds[$i]}

    build_location="repos/$repo/builds"

    # Get the status of the last build from the repo
    json=$(curl --silent -sb -H "Accept: application/json" -H "Authorization: $TOKEN" -X GET "$DRONE_URL/api/$build_location")

    result=$(echo "$json" | jq ".[] | select(.number==$build) | {status: .status}" | grep "status" | awk '{print $2}' | head -n 1)
    result=${result:1:${#result}-2}

    case $result in
    "success")
    output=$output"\\n$repo #$build: $result | color=#00bfa5 href=$DRONE_URL/$repo/$build"
    success=$((success + 1))
    ;;
    "failure")
        output=$output"\\n$repo #$build: $result | color=#f50057 href=$DRONE_URL/$repo/$build"
        failure=$((failure + 1))
        ;;
    "running")
        output=$output"\\n$repo #$build: $result | color=#ffa000 href=$DRONE_URL/$repo/$build"
        running=$((running + 1))
        ;;
    esac
done

result_color=#00bfa5

if [[ $failure -gt 0 ]]; then
result_color=#f50057
fi

echo -e "Drone CI | color=$result_color\\n---\\n$output"

