#!/bin/bash
# <xbar.title>Check status of single Jenkins project</xbar.title>
# <xbar.author>Stratouklos</xbar.author>
# <xbar.author.github>Stratouklos</xbar.author.github>

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
