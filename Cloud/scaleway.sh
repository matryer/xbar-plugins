#!/bin/bash

# <xbar.title>Scaleway Instances</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Nick Penkov</xbar.author>
# <xbar.author.github>npenkov</xbar.author.github>
# <xbar.desc>Let you start/stop scaleway server instances</xbar.desc>
# <xbar.dependencies>scw,jq</xbar.dependencies>

# Dependencies:
# [scw](https://github.com/scaleway/scaleway-cli)
# [jq](https://stedolan.github.io/jq/)

# Installation:
# 1. Copy this script to your BitBar plugin folder
# 2. Ensure the plugin file is executable by running chmod +x scaleway.sh
# 3. Change your SCW profile settings
SCW_CLI_PROFILE="default"

CMD_SCW=$(command -v /usr/local/bin/scw)
CMD_JQ=$(command -v /usr/local/bin/jq)

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
      echo "--stop | bash='$0' bash=$CMD_SCW param1=instance param2=server param3=stop param4=$instance_id refresh=false terminal=false"
    elif [ "$status_info" == "stopping" ]; then
      echo "--status: $status_info"
    elif [ "$status_info" == "starting" ]; then
      echo "--status: $status_info"
    else
      echo "--status: $status_info"
      echo "--start | bash='$0' bash=$CMD_SCW param1=instance param2=server param3=start param4=$instance_id refresh=false terminal=false"
    fi
  done
}

main() {
  print_instance 

  if [ "$ARG1_ACTION" ]; then
    osascript -e "display notification \"$ARG1_ACTION\" "
    $CMD_SCW -p$SCW_CLI_PROFILE instance server $ARG1_ACTION $INSTANCE_ID
  fi
}

echo "SCW"
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
