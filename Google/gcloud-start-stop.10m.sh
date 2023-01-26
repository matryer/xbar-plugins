#!/bin/bash

# <xbar.title>Google Cloud Instance Start/Stop</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gregory Senay</xbar.author>
# <xbar.author.github>Gregory Senay</xbar.author.github>
# <xbar.desc>Let you start/stop Google instances; Inspired by Amazon EC2 Instance Start/Stop plugin</xbar.desc>
# <xbar.image>https://i.imgur.com/1AnOUGG.png</xbar.image>
# <xbar.dependencies>gcloud</xbar.dependencies>

# Dependencies:
# gcloud  ( https://cloud.google.com/sdk/docs/install/)

# Installation:
# 1. Copy this script to your xBar plugin folder
# 2. Ensure the plugin file is executable by running chmod +x gcloud-start-stop.10m.sh
# 3. Insure the location of GCLOUD_SDK, else change it 

GCLOUD_SDK=$HOME/.google-cloud-sdk

# The next line updates PATH for the Google Cloud SDK.
if [ -f $GCLOUD_SDK/path.bash.inc ]; then . $GCLOUD_SDK/path.bash.inc; fi

CMD_GCLOUD=$(command -v $GCLOUD_SDK/bin/gcloud)
DISABLED_ITEM_COLOR="#C0C0C0"
ARG1_NAME="$1"
ARG2_ACTION="$2"
ARG3_ZONE="$3"
STATUS_STOPPED="🍎"
STATUS_STARTED="🍏"

# function print_instance()
# print out menu for each instance
# parameters:
#   $1, instance details in format:
#   "<instance_name>__--SEP--__<instance_status>"

print_instance(){
  instance_name=$(echo "$1" | awk -F "__--SEP--__" '{print $1}')
  instance_status=$(echo "$1" | awk -F "__--SEP--__" '{print $6}')
  zone=$(echo "$1" | awk -F "__--SEP--__" '{print $2}')

    if [ "$instance_status" = "RUNNING" ]; then
      echo "$STATUS_STARTED $instance_name"
    else
      echo "$STATUS_STOPPED $instance_name"
    fi

    echo "--status: $instance_status"
    if [ "$instance_status" = "RUNNING" ]; then
      echo "--start | color=$DISABLED_ITEM_COLOR"
      echo "--stop | bash='$0' param1=$instance_name param2=stop param3=$zone refresh=false terminal=false"
    elif [ "$instance_status" = "TERMINATED" ]; then
      echo "--start | bash='$0' param1=$instance_name param2=start param3=$zone refresh=false terminal=false"
      echo "--stop | color=$DISABLED_ITEM_COLOR"
    fi
}

main() {
  instances=$( gcloud compute instances list|tail -n +2| sed -E "s/[ ]+/__--SEP--__/g" )
  for row in $instances; do
    print_instance "$row"
  done

  if [ "$ARG2_ACTION" ]; then
    CMD_NOTIFY=$(command -v osascript)
    if [ "$CMD_NOTIFY" ]; then
      osascript -e "display notification \"$ARG1_NAME $ARG2_ACTION $ARG3_ZONE\" "
    fi
    $CMD_GCLOUD compute instances "$ARG2_ACTION" "$ARG1_NAME" --zone=$ARG3_ZONE
  fi
}

echo "GCloud Instances"
echo "---"

if [ "$CMD_GCLOUD" ] ; then
  main
else
  echo "gcloud command not found"
fi
echo "Refresh status | refresh=true"

