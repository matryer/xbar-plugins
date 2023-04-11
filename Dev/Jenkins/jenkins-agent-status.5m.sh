#!/usr/bin/env bash

# <xbar.title>Jenkins Agent Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author.github>avidit</xbar.author.github>
# <xbar.desc>Monitor status of jenkins agents</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/avidit/my-xbar-plugins/develop/jenkins/jenkins.png</xbar.image>
# <xbar.dependencies>jq</xbar.dependencies>

# Variables:
# <xbar.var>string(JENKINS_URL="https://ci.jenkins.io"): Jenkins URL</xbar.var>
# <xbar.var>string(JENKINS_AGENTS="AGENT_01,AGENT_02,AGENT_03"): Jenkins Agent(s)</xbar.var>
# <xbar.var>string(JENKINS_USER_ID=""): Jenkins user id</xbar.var>
# <xbar.var>string(JENKINS_API_TOKEN=""): Jenkins API Token</xbar.var>

# Dependencies:
# jq (https://stedolan.github.io/jq/)

# Installation:
# 1. Copy this script to xbar plugin folder ~/Library/Application Support/xbar/plugins
# 2. Ensure the plugin file is executable by running chmod +x jenkins-agent-status.5m.sh

echo "💻"
echo "---"

[ -n "$JENKINS_URL" ] || { echo "❕ JENKINS_URL not set"; exit; }
[ -n "$JENKINS_AGENTS" ] || { echo "❕ JENKINS_AGENTS not set"; exit; }
[ -n "$JENKINS_USER_ID" ] || { echo "❕ JENKINS_USER_ID not set"; exit; }
[ -n "$JENKINS_API_TOKEN" ] || { echo "❕ JENKINS_API_TOKEN not set"; exit; }

function check_status() {
    AGENT=$1
    STATUS_URL="$JENKINS_URL/computer/$AGENT/api/json"
    RESPONSE=$(curl --silent --user "$JENKINS_USER_ID:$JENKINS_API_TOKEN" "$STATUS_URL")
    OFFLINE=$(echo "$RESPONSE" | /usr/local/bin/jq -r '.offline')
    REASON=$(echo "$RESPONSE" | /usr/local/bin/jq -r '.offlineCauseReason')
    if [[ "$OFFLINE" == "false" ]];
    then
        echo "✅ $AGENT: Online | href=${JENKINS_URL}/computer/$AGENT/"
    elif [[ "$OFFLINE" == "true" ]];
    then
        echo "❌ $AGENT: Offline | href=${JENKINS_URL}/computer/$AGENT/"
        echo "-- ${REASON//$'\n'*/ }"
    else
        echo "❓ $AGENT: Unknown | href=${JENKINS_URL}/computer/$AGENT/"
    fi
}

IFS=', ' read -r -a AGENTS <<< "$JENKINS_AGENTS"
for AGENT in "${AGENTS[@]}"
do
    check_status "$AGENT"
done
