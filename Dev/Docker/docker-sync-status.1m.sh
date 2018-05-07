#!/bin/bash
#
# <bitbar.title>Docker-Sync Status</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Brendan Shanny</bitbar.author>
# <bitbar.author.github>brenshanny</bitbar.author.github>
# <bitbar.desc>Display the current status of docker-sync.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/LbaqXzU.png</bitbar.image>
# <bitbar.dependencies>docker-sync</bitbar.dependencies>
#
# Docker-Sync Status Plugin
#
# by Brendan Shanny
#
# To get started, create a new text file in your project tree that docker-sync will watch
# then edit the docker_log and docker_checker vars below to be the paths to your
# docker_sync daemon.log and the newly created text file.

error_status="❌"
error_message="Docker-Sync does not appear to by syncing | color=#ff0000"

success_status="🛰"
success_message="Docker-Sync is Running | color=#008000"

# The path to your docker-sync daemon.log file
docker_log="/path/to/your/project/.docker-sync/daemon.log"
# The path to a text file within your project that docker-sync will watch
docker_checker="/path/to/your/project/docker-sync-checker.txt"

first_file_access=$(stat -f '%m' $docker_log)

# Trigger a sync of the file
echo "test docker-sync" >> $docker_checker

# 3s works just fine, 5s provides a safety margin
sleep 5

second_file_access=$(stat -f '%m' $docker_log)

# Reset the file
echo "This file is used by the bitbar docker-sync-status plugin to check the status of docker-sync." > $docker_checker

if [ "$first_file_access" == "$second_file_access" ]; then
  sync_status=$error_status
  sync_message=$error_message
else
  sync_status=$success_status
  sync_message=$success_message
fi
echo "$sync_status"
echo "---"
echo "$sync_message"
