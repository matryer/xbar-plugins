#!/bin/sh

# <bitbar.title>Amazon EC2 Instance Start/Stop</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Francis Mak</bitbar.author>
# <bitbar.author.github>franfran</bitbar.author.github>
# <bitbar.desc>Let you start/stop Amazon EC2 instances</bitbar.desc>
# <bitbar.dependencies>awscli,jq</bitbar.dependencies>

# Dependencies:
# awscli (https://aws.amazon.com/cli/)
# jq (https://stedolan.github.io/jq/)

# Installation:
# 1. Copy this script to your BitBar plugin folder
# 2. Ensure the plugin file is executable by running chmod +x ec2-start-stop.1h.sh
# 3. Change your AWS profile in the AWS_CLI_PROFILE variable below
# Notes: Optionally, to display the instace name in bitbar, use the key "Name" in AWS EC2 Tags
AWS_CLI_PROFILE="default"

export PATH="$PATH:/usr/local/bin"

CMD_AWS=$(command -v aws)
CMD_JQ=$(command -v jq)
DISABLED_ITEM_COLOR="#C0C0C0"
ARG1_INSTANCE_ID="$1"
ARG2_NAME="$2"
ARG3_ACTION="$3"
STATUS_STOPPED="🍎"
STATUS_STARTED="🍏"

# function print_instance()
# print out menu for each instance
# parameters:
#   $1, instance details in format:
#   "<instance_id>__--SEP--__<instance_tag_name>__--SEP--__<instance_status>"
print_instance(){
  instance_id=$(echo "$1" | awk -F "__--SEP--__" '{print $1}')
  instance_tag_name=$(echo "$1" | awk -F "__--SEP--__" '{print $2}')
  instance_status=$(echo "$1" | awk -F "__--SEP--__" '{print $3}')
  
  if [ "$instance_status" = "running" ]; then
    echo "$STATUS_STARTED $instance_tag_name"
  else
    echo "$STATUS_STOPPED $instance_tag_name"
  fi
  echo "----"
  echo "--status: $instance_status"
  if [ "$instance_status" = "running" ]; then
    echo "--start | color=$DISABLED_ITEM_COLOR"
    echo "--stop | bash='$0' param1=$instance_id param2=$instance_tag_name param3=stop-instances refresh=false terminal=false"
  elif [ "$instance_status" = "stopped" ]; then
    echo "--start | bash='$0' param1=$instance_id param2=$instance_tag_name param3=start-instances refresh=false terminal=false"
    echo "--stop | color=$DISABLED_ITEM_COLOR"
  fi
}

main() {
  json=$( $CMD_AWS --profile $AWS_CLI_PROFILE ec2 describe-instances --output json )
  #shellcheck disable=SC2016
  instances=$( echo "$json" | $CMD_JQ -r 'def count(s): reduce s as $_ (0;.+1);.Reservations | .[] | .Instances | .[] | .InstanceId as $i | $i +"__--SEP--__" + (if count (.Tags[]? | select(.Key=="Name")) == 0 then $i else (.Tags[] | select(.Key=="Name")|.Value) end) +"__--SEP--__" + .State.Name' )
  #if no Name tag found, it will use instance id instead

  for row in $instances; do
    print_instance "$row"
  done

  if [ "$ARG3_ACTION" ]; then
    CMD_NOTIFY=$(command -v osascript)
    if [ "$CMD_NOTIFY" ]; then
      osascript -e "display notification \"$ARG2_NAME $ARG3_ACTION\" "
    fi
    $CMD_AWS --profile $AWS_CLI_PROFILE ec2 "$ARG3_ACTION" --instance-ids "$ARG1_INSTANCE_ID"
  fi
}

echo "EC2 Instances"
echo "---"

if [ "$CMD_AWS" ] && [ "$CMD_JQ" ]; then
  main
else
  echo "aws/jq command not found"
fi
echo "Refresh status | refresh=true"