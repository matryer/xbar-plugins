#!/usr/bin/env bash

# <bitbar.title>List running Kubernetes pods (default namespace)</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Robert Prince</bitbar.author>
# <bitbar.author.github>robertp</bitbar.author.github>
# <bitbar.desc>Simple that shows running Kubernetes pods; it's just output from 'kubectl get pods' with a font specified. It assumes you have installed kubectl using brew.</bitbar.desc>
# <bitbar.dependencies>brew,kubectl</bitbar.dependencies>

export PATH=/usr/local/bin:"${PATH}"

echo "k8s pods"
echo "---"
kubectl get pods | while read -r line; do echo "${line} | font=Menlo"; done
