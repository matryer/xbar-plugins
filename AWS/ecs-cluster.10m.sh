#!/bin/sh
# <xbar.title>Amazon ECS Cluster Status</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Bob Zoller</xbar.author>
# <xbar.author.github>bobzoller</xbar.author.github>
# <xbar.desc>Shows statistics about your Amazon ECS cluster.</xbar.desc>
# <xbar.dependencies>awscli,jq</xbar.dependencies>

# Dependencies: 
#   awscli (https://aws.amazon.com/cli/)
#   jq (https://stedolan.github.io/jq/)

AWS_CLI_PROFILE="default"
ECS_CLUSTER=""

export PATH="$PATH:/usr/local/bin"

if [ -z "$ECS_CLUSTER" ]; then
  echo "Missing configuration: cluster name"
  exit 1
fi

data=$( aws --profile $AWS_CLI_PROFILE ecs list-container-instances --output json --cluster $ECS_CLUSTER )
instance_ids=$( echo "$data" | jq -r '.containerInstanceArns | map(. | split("/") | last) | join(" ")' )

data=$( aws --profile $AWS_CLI_PROFILE ecs describe-container-instances --output json --cluster $ECS_CLUSTER --container-instances "$instance_ids" )
instance_count=$( echo "$data" | jq -r '.containerInstances | length' )

total_memory=$(( $( echo "$data" | jq -r '.containerInstances | map(.registeredResources[] | select(.name == "MEMORY") | .integerValue) | add' ) / 1000 ))
remaining_memory=$(( $( echo "$data" | jq -r '.containerInstances | map(.remainingResources[] | select(.name == "MEMORY") | .integerValue) | add' ) / 1000 ))
reserved_memory=$(( total_memory - remaining_memory ))
usage_memory=$( echo "$reserved_memory * 100 / $total_memory" | bc )

total_cpu=$( echo "$data" | jq -r '.containerInstances | map(.registeredResources[] | select(.name == "CPU") | .integerValue) | add' )
remaining_cpu=$( echo "$data" | jq -r '.containerInstances | map(.remainingResources[] | select(.name == "CPU") | .integerValue) | add' )
reserved_cpu=$(( total_cpu - remaining_cpu ))
usage_cpu=$( echo "$reserved_cpu * 100 / $total_cpu" | bc )

mono() {
  echo "$1 | font=Monaco trim=false"
}

echo "#:${instance_count} C:${usage_cpu}% M:${usage_memory}%"
echo "---"
mono "Instances:"
mono "  Count:    ${instance_count}"
mono "Memory:"
mono "  Total:    ${total_memory} GB"
mono "  Reserved: ${reserved_memory} GB"
mono "  Free:     ${remaining_memory} GB"
mono "CPU:"
mono "  Total:    ${total_cpu} shares"
mono "  Reserved: ${reserved_cpu} shares"
mono "  Free:     ${remaining_cpu} shares"

