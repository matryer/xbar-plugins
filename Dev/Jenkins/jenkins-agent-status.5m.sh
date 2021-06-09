#!/bin/sh

# <xbar.title>Jenkins Agent Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Aabishkar kc</xbar.author>
# <xbar.author.github>avidit</xbar.author.github>
# <xbar.desc>Monitor status of jenkins agents</xbar.desc>
# <xbar.dependencies>jq</xbar.dependencies>

# Dependencies:
# jq (https://stedolan.github.io/jq/)

JENKINS_URL="https://www.jenkins.io"
JENKINS_USER="user"
JENKINS_TOKEN="token"
JENKINS_AGENTS=("AGENT_01" "AGENT_02")

echo "💻"
echo '---'

[ ! -z "$JENKINS_URL" ] || { echo "❕ JENKINS_USER not set"; exit; }
[ ! -z "$JENKINS_USER" ] || { echo "❕ JENKINS_USER not set"; exit; }
[ ! -z "$JENKINS_TOKEN" ] || { echo "❕ JENKINS_TOKEN not set"; exit; }
[ ! -z "$JENKINS_AGENTS" ] || { echo "❕ JENKINS_AGENTS not set"; exit; }

function check_status() {
    AGENT=$1
    OFFLINE=$(curl --silent --user $JENKINS_USER:$JENKINS_TOKEN $JENKINS_URL/computer/$AGENT/api/json | /usr/local/bin/jq '.offline')
    if [[ "$OFFLINE" == "false" ]];
    then
        echo "✅ $AGENT: Online"
    elif [[ "$OFFLINE" == "true" ]];
    then
        echo "❌ $AGENT: Offline"
    else
        echo "❓ $AGENT: Unknown"
    fi
}

for AGENT in "${JENKINS_AGENTS[@]}"
do
    check_status $AGENT
done
