#!/usr/bin/env bash

# <xbar.title>Jenkins Agent Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Aabishkar kc</xbar.author>
# <xbar.author.github>avidit</xbar.author.github>
# <xbar.desc>Monitor status of jenkins agents</xbar.desc>
# <xbar.dependencies>jq</xbar.dependencies>

# Dependencies:
# jq (https://stedolan.github.io/jq/)

JENKINS_URL="https://ci.jenkins.io"
JENKINS_AGENTS=("AGENT_01" "AGENT_02")
JENKINS_USER_ID="user"
JENKINS_API_TOKEN="token"
JQ=$(command -v jq)

echo "💻"
echo "---"

[ -n "$JENKINS_URL" ] || { echo "❕ JENKINS_USER_ID not set"; exit; }
[ -n "$JENKINS_AGENTS" ] || { echo "❕ JENKINS_AGENTS not set"; exit; }
[ -n "$JENKINS_USER_ID" ] || { echo "❕ JENKINS_USER_ID not set"; exit; }
[ -n "$JENKINS_API_TOKEN" ] || { echo "❕ JENKINS_API_TOKEN not set"; exit; }

function check_status() {
    AGENT=$1
    STATUS_URL="$JENKINS_URL/computer/$AGENT/api/json"
    RESPONSE=$(curl --silent --user $JENKINS_USER_ID:$JENKINS_API_TOKEN "$STATUS_URL")
    OFFLINE=$(echo "$RESPONSE" | $JQ -r '.offline')
    REASON=$(echo "$RESPONSE" | $JQ -r '.offlineCauseReason')
    if [[ "$OFFLINE" == "false" ]];
    then
        echo "✅ $AGENT: Online"
    elif [[ "$OFFLINE" == "true" ]];
    then
        echo "❌ $AGENT: Offline"
        echo "-- ${REASON//$'\n'*/ }"
    else
        echo "❓ $AGENT: Unknown"
    fi
}

for AGENT in "${JENKINS_AGENTS[@]}"
do
    check_status "$AGENT"
done

echo ---
echo "Refresh... | refresh=true"
