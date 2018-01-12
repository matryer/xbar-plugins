#!/bin/bash
#
# metadata
# <bitbar.title>jira-search</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Padraic Renaghan</bitbar.author>
# <bitbar.author.github>prenagha</bitbar.author.github>
# <bitbar.desc>Display results from JIRA search</bitbar.desc>
# <bitbar.dependencies>jq command line json processor https://stedolan.github.io/jq</bitbar.dependencies>

# your JIRA user id
USER="userid"

# your JIRA password
PASSWORD="pwd"

# the base URL to your JIRA install, like http://example.net/jira/
JIRA_BASE_URL="https://example.net/jira"

# JQL query you want this script to run
QUERY="assignee = currentUser() AND status != Closed ORDER BY updated"

# jq command line json processor
# https://stedolan.github.io/jq
JQ="/usr/local/bin/jq"

# you can add --insecure if your JIRA install does not have a real SSL certificate
json=$(/usr/bin/curl --fail --silent --get \
  --header "Accept: application/json" \
  --user "${USER}:${PASSWORD}" \
  --data-urlencode "fields=key,summary" \
  --data-urlencode "jql=${QUERY}" \
  --url "${JIRA_BASE_URL}/rest/api/2/search" )

total=$(echo "$json" | $JQ --raw-output ".total")
if [ "${total}" -gt 0 ]
then
  echo "${total}🗳"
  echo "---"
  i="0"
  while [ $i -lt "${total}" ]
  do
    key=$(echo "$json" | $JQ --raw-output ".issues[${i}].key")
    summary=$(echo "$json" | $JQ --raw-output ".issues[${i}].fields.summary")
    url="${JIRA_BASE_URL}/browse/${key}"
    echo "${key}: ${summary} | length=50 href=$url"
    ((i+=1))
  done
fi

exit 0