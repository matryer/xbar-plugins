#!/usr/bin/env bash

# <bitbar.title>List some running Kubernetes things</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Robert Prince</bitbar.author>
# <bitbar.author.github>robertp</bitbar.author.github>
# <bitbar.desc>Simple plugin that shows running Kubernetes pods, services, deployments, ...</bitbar.desc>
# <bitbar.dependencies>brew,kubectl</bitbar.dependencies>
# <bitbar.image>https://i.imgur.com/sH9yhBW.png</bitbar.image>

export PATH=/usr/local/bin:"${PATH}"

numpods=$(kubectl get pods -A 2> /dev/null | grep -v NAME | wc -l | sed 's/ //g')
numsvc=$(kubectl get services -A 2> /dev/null | grep -v NAME | wc -l | sed 's/ //g')
numdeps=$(kubectl get deployments -A 2> /dev/null | grep -v NAME | wc -l | sed 's/ //g')

# if [[ "$numpods" -eq "0" && "$numsvc" -eq "0" && "$numdeps" -eq "0" ]]; then echo "no k8s"; exit; fi

if [[ "$numpods" -eq "0" && "$numsvc" -eq "0" && "$numdeps" -eq "0" ]]; then exit; fi

echo "[$numpods pods / $numsvc services / $numdeps deployments]"

echo "---"
echo "==== PODS ===="
kubectl get pods -A | while read -r line; do echo "${line} | font=Menlo"; done
echo "---"
echo "==== SERVICES ===="
kubectl get services -A | while read -r line; do echo "${line} | font=Menlo"; done
echo "---"
echo "==== DEPLOYMENTS ===="
kubectl get deployments -A | while read -r line; do echo "${line} | font=Menlo"; done
