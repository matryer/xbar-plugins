#!/usr/bin/env bash

# <xbar.title>Kubeconfig Namespace & Context Switcher</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Elias Abacioglu</xbar.author>
# <xbar.author.github>Raboo</xbar.author.github>
# <xbar.desc>Displays active kubeconfig namespace and context, also allows you to switch namespace or context.</xbar.desc>
# <xbar.dependencies>bash,kubectl</xbar.dependencies>
# <xbar.image>https://i.imgur.com/AmBpdmp.png</xbar.image>

# KUBECONFIG=~/.kube/config # Uncomment and set this if you need a custom kube config path.
PATH=/opt/homebrew/bin:/usr/local/bin:$PATH
KUBECTL=$(command -v kubectl)
KUBERNETES_LOGO='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'

currentNamespace=$(kubectl config view --minify --output 'jsonpath={..namespace}')
currentContext=$(kubectl config current-context)

namespaces=$(kubectl get namespaces --no-headers -o custom-columns=NAME:.metadata.name)
contexts=$(kubectl config get-contexts --no-headers -o name)


echo "|image=${KUBERNETES_LOGO}"
echo "---"

echo "Namespace: ${currentNamespace}"
for item in ${namespaces}; do
  echo -n "--${item} | bash=${KUBECTL} param1=config param2=set-context param3=--current param4='--namespace=${item}' terminal=false refresh=true"
  if [ "${item}" == "${currentNamespace}" ]; then
    echo ' color=red'
  else
    echo
  fi
done

echo "Context: ${currentContext}"
for item in ${contexts}; do
  echo -n "--${item} | bash=${KUBECTL} param1=config param2=use-context param3=${item} terminal=false refresh=true"
  if [ "${item}" == "${currentContext}" ]; then
    echo ' color=red'
  else
    echo
  fi
done
