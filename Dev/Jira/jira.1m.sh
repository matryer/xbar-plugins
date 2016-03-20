#!/bin/bash

# bitbar plugins for devs that use https://www.atlassian.com/software/jira
#
# gets all assigned tasks on open stories for users.
# also shows when there are open blockers
#
# metadata
# <bitbar.title>jira</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Nicolas Gehlert</bitbar.author>
# <bitbar.author.github>ngehlert</bitbar.author.github>
# <bitbar.desc>display all assigned tasks, all open stories and open blockers.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/HwBF2JX.png</bitbar.image>

USER=""
PASSWORD=""
JIRA_BASE_URL="https://example.com/jira"
PROJECT="PROJECT"
COOKIE_LOCATION="$HOME/.jira-cookie"

function getJsonValue() {
  KEY=$1
  num=$2
  awk -F"[,:}]" '{for(i=1;i<=NF;i++){if($i~/'"$KEY"'\042/){print $(i+1)}}}' | tr -d '"' | sed -n "${num}"p
}
# stupid hack because json parsing in bash sucks :D find better solution later
function getTextWithCommaJsonValue() {
  KEY=$1
  num=$2
  awk -F"[:}]" '{for(i=1;i<=NF;i++){if($i~/'"$KEY"'\042/){print $(i+1)}}}' | tr -d '"' | sed -n "${num}"p
}

function echoAllIssues() {
  json=$1
  numberOfTasks=$2
  color=$3
  i="1"
  while [ $i -le "$numberOfTasks" ]
  do
    issues=$(echo "$json" | getTextWithCommaJsonValue summary $i)
    key=$(echo "$json" | getJsonValue key $i)
    url="${JIRA_BASE_URL}/browse/${key}"
    echo "$key: $issues | bash=$0 param1=$url terminal=false color=$color"
    ((i+=1))
  done
}

function login() {
  curl --insecure --silent -X POST -H "Content-Type: application/json" -H "X-Atlassian-Token: nocheck" -c "${COOKIE_LOCATION}" --data "{\"username\":\"${USER}\",\"password\":\"${PASSWORD}\"}" "${JIRA_BASE_URL}/rest/auth/1/session"
}

function run() {
  recursive=$1
  blocker=$(curl --insecure --silent -X GET -b "${COOKIE_LOCATION}" -H "Content-Type: application/json" $JIRA_BASE_URL/rest/api/2/search?jql=sprint%20in%20openSprints\(\)AND%20priority=Blocker%20AND%20project=${PROJECT}%20AND%20status\!=done\&fields=summary)

  assignedTasks=$(curl --insecure --silent -X GET -b "${COOKIE_LOCATION}" -H "Content-Type: application/json" $JIRA_BASE_URL/rest/api/2/search?jql=assignee%20in\(currentUser\(\)\)AND%20sprint%20in%20openSprints\(\)AND%20status\!=done\&fields=summary)

  stories=$(curl  --insecure --silent -X GET -b "${COOKIE_LOCATION}" -H "Content-Type: application/json" $JIRA_BASE_URL/rest/api/2/search?jql=sprint%20in%20openSprints\(\)%20AND%20issuetype=story%20AND%20project=${PROJECT}%20AND%20status\!=done\&fields=summary)

  totalBlocker=$(echo "$blocker" | getJsonValue total 1)
  totalAssignedTasks=$(echo "$assignedTasks" | getJsonValue total 1)
  totalStories=$(echo "$stories" | getJsonValue total 1)
  if [[ -z "$totalBlocker" || -z "$totalAssignedTasks" || -z "$totalStories" ]]
    then
      # if data loading failed on the second attempt there is probably a bigger issue
      if [[ "$recursive" = true ]]
        then
          echo There seems to be a problem with the jira login. Check your configuration
          exit
      fi
      # token is expired. login again and try to load data
      login
      run true
  fi

  if [ "$totalBlocker" -gt 0 ]
    then
      echo "Blocker: ${totalBlocker} | color=#F44336"
    else
      echo "Tasks assigned: ${totalAssignedTasks} | color=#333333"
  fi

  echo "---"

  echo "Blocker: ${totalBlocker} | color=#333333"
  echoAllIssues "$blocker" "$totalBlocker" "#F44336"
  echo "---"

  echo "Tasks assigned: ${totalAssignedTasks} | color=#333333"
  echoAllIssues "$assignedTasks" "$totalAssignedTasks" "#333333"
  echo "---"

  echo "Open Stories: ${totalStories} | color=#333333"
  echoAllIssues "$stories" "$totalStories" "#333333"

  exit
}

# the program starts here

# opens jira issue page if one entry was clicked
if [[ "$1" == http* ]]; then
  open "$1"
  exit
fi

# only login if there is not already a auth token
if [ ! -f "${COOKIE_LOCATION}" ]
  then
    login
fi

run
