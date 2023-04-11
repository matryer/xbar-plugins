#!/bin/bash

# <xbar.title>Scaleway Instances</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Nick Penkov</xbar.author>
# <xbar.author.github>npenkov</xbar.author.github>
# <xbar.desc>Let you start/stop scaleway server instances</xbar.desc>
# <xbar.dependencies>scw,jq</xbar.dependencies>
# <xbar.image>https://i.imgur.com/GgYNzMQ.png</xbar.image>

# Dependencies:
# [scw](https://github.com/scaleway/scaleway-cli)
# [jq](https://stedolan.github.io/jq/)

# Installation:
# 1. Copy this script to your BitBar plugin folder
# 2. Ensure the plugin file is executable by running chmod +x scaleway.20s.sh
# 3. Change your SCW profile settings
unset SCW_ACCESS_KEY SCW_SECRET_KEY SCW_DEFAULT_ORGANIZATION_ID SCW_DEFAULT_REGION SCW_DEFAULT_ZONE SCW_PROFILE
SCW_CLI_PROFILE="default"
ACTION="$1"
INSTANCE="$2"
SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"


BREWPATH=/usr/local/bin
if [[ $(sysctl -n machdep.cpu.brand_string) =~ "Apple" ]]; then
  BREWPATH=/opt/homebrew/bin
fi

CMD_SCW=$(command -v ${BREWPATH}/scw)
CMD_JQ=$(command -v ${BREWPATH}/jq)

export PATH="$PATH:/usr/local/bin"

DISABLED_ITEM_COLOR="#C0C0C0"
STATUS_STOPPED="🔴"
STATUS_STARTED="🟢"
STATUS_STOPPING="🟠"
STATUS_STARTING="🔵"

print_instance(){
  instances=$($CMD_SCW -p$SCW_CLI_PROFILE instance server list -o json=pretty | jq -r '.[] | (.id +"="+ .name +"="+.state)')
  for instance in $instances; do 
    instance_id=$(echo "$instance" | cut -d'=' -f1)
    instance_name=$(echo "$instance" | cut -d'=' -f2)
    status_info=$(echo "$instance" | cut -d'=' -f3)
    if [ "$status_info" == "running" ]; then
      echo "$STATUS_STARTED $instance_name"
    elif [ "$status_info" == "stopping" ]; then
      echo "$STATUS_STOPPING $instance_name"
    elif [ "$status_info" == "starting" ]; then
      echo "$STATUS_STARTING $instance_name"
    else
      echo "$STATUS_STOPPED $instance_name"
    fi
    # echo "--"
    if [ "$status_info" == "running" ]; then
      echo "--status: $status_info"
      echo "--stop | bash='$SCRIPTPATH/$0' param1=stop param2=$instance_id refresh=false terminal=false"
    elif [ "$status_info" == "stopping" ]; then
      echo "--status: $status_info"
    elif [ "$status_info" == "starting" ]; then
      echo "--status: $status_info"
    else
      echo "--status: $status_info"
      echo "--start | bash='$SCRIPTPATH/$0' param1=start param2=$instance_id refresh=false terminal=false"
    fi
  done
}

main() {
  print_instance 

  if [ ! -z "$ACTION" ]; then
    $CMD_SCW -p$SCW_CLI_PROFILE instance server $ACTION $INSTANCE
  fi
}

echo "☁️ "
echo "---"

if [ "$CMD_SCW" ]; then
  if [ "$CMD_JQ" ]; then
    main
  else
    echo "jq command not found - ${CMD_JQ}"
  fi
else
  echo "scw command not found - ${CMD_SCW}"
fi
echo "Refresh status | refresh=true"
