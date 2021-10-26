#!/usr/bin/env bash

# <xbar.title>List some running Kubernetes things</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Robert Prince</xbar.author>
# <xbar.author.github>robertp</xbar.author.github>
# <xbar.desc>Simple plugin that shows running Kubernetes pods, services, deployments, ...</xbar.desc>
# <xbar.dependencies>brew,kubectl</xbar.dependencies>
# <xbar.image>https://i.imgur.com/sH9yhBW.png</xbar.image>

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
