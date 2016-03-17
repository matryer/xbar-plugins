#!/bin/bash
# <bitbar.title>Check status of single Jenkins project</bitbar.title>
# <bitbar.author>Stratouklos</bitbar.author>
# <bitbar.author.github>Stratouklos</bitbar.author.github>

USER="username"
PASS="pass"
BASE_URL="my-jenkins.com"
JOBNAME="jobname"

RESULT=$(curl -silent http://${USER}:${PASS}@${BASE_URL}/job/${JOBNAME}/lastBuild/api/json?pretty=true | grep "result" | awk '{print $3}')

if [[ $RESULT == *"SUCCESS"* ]]
then
  echo '🍏'
else
  echo '🍎'
fi
