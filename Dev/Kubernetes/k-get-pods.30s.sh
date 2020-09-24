#!/usr/bin/env bash

# <bitbar.title>List running Kubernetes pods (default namespace)</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Robert Prince</bitbar.author>
# <bitbar.author.github>robertp</bitbar.author.github>
# <bitbar.desc>Simple that shows running Kubernetes pods; it's just output from 'kubectl get pods' with a font specified. It assumes you have installed kubectl using brew.</bitbar.desc>
# <bitbar.dependencies>brew,kubectl</bitbar.dependencies>

export PATH=/usr/local/bin:"${PATH}"

numpods=$(kubectl get pods -A 2> /dev/null | grep -v NAME | wc -l)
echo "$numpods pods running"
echo "---"
if [[ "$numpods" -eq 0 ]]; then exit; fi
kubectl get pods -A | while read -r line; do echo "${line} | font=Menlo"; done
