#!/bin/bash
USER="username"
PASS="password"
BASE_URL="jenkins-address.com"
PROJECTS=("project1" "project2")

function displaytime {
  local T=$1/1000
  local D=$((T/60/60/24))
  local H=$((T/60/60%24))
  local M=$((T/60%60))
  local S=$((T%60))
  local output=""

  if [[ $D -gt 0  || $H -gt 0 || $M -gt 10 ]]
  then
    output+=">10mn"
  else
    output+="${M}mn ${S}s"
  fi

  echo "${output} ago"
}

# beginning of display
echo "Jenkins Status"
echo "---"

for project in "${PROJECTS[@]}"
do
  output="${project}: "
  url="https://${USER}:${PASS}@${BASE_URL}/job/${project}/lastBuild/api/json?pretty=true"
  query=$(curl --insecure --silent "${url}" | tail -30) # take only the end of output

  success=$(echo "${query}" | grep "result" | awk '{print $3}') # grep the "result" line

  if [[ $success == *"SUCCESS"* ]]
  then
    output+='🔵 '
  else
    output+='🔴 '
  fi

  timestamp=$(echo "${query}" | grep "timestamp" | awk '{print $3}') # grep the "timestamp" line
  timestamp=${timestamp%?} # remove the trailing ','
  currentTime=$(($(date +'%s * 1000 + %-N / 1000000'))) # generate a timestamp
  output+=" $(displaytime $(( currentTime - timestamp )))"
  echo "${output}"
done
