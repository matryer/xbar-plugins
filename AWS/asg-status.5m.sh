#!/bin/zsh
#  <xbar.title>AWS ASG Health Monitor</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>John Brahy</xbar.author>
#  <xbar.author.github>jbrahy</xbar.author.github>
#  <xbar.desc>This plugin shows all of your application scaling groups and details on their health.
#  In the array ORDERED_ARGS, add the names of each of the ASGs and then a pipe symbol | with the 
#  friendly name you want to display next to it. The order of the items in the array will be the 
#  order of the items in the dropdown.
#  i.e. production-app-server-asg|Production App Servers
#  If you want to ignore ASGs then add them to the IGNORED_ASGS array.
#  </xbar.desc>
#  <xbar.dependencies>zsh,aws cli,jq</xbar.dependencies>
#  <xbar.var>string(VAR_AWS_PROFILE="default"): AWS Profile name.</xbar.var>
#  <xbar.var>string(VAR_REGION="us-east-1"): AWS Region code.</xbar.var>
#  <xbar.var>string(VAR_ORDERED_ASGS="production-app-server-asg:Production App Servers"): Comma separated list of ASG names in the order to display along with their friendly name after a pipe .</xbar.var>
#  <xbar.var>string(VAR_IGNORED_ASGS="asg-to-ignore,other-asg-to-ignore"): Comma separated list of ASG names not to display.</xbar.var>

# AWS CLI Profile & Region
AWS_PROFILE=$VAR_AWS_PROFILE
REGION=$VAR_REGION

# Define the ordered list of ASGs with their display names
ORDERED_ASGS=(${(@s/,/)VAR_ORDERED_ASGS})

# Define ASGs to ignore
IGNORED_ASGS=(${(@s/,/)VAR_IGNORED_ASGS})

# Fetch Auto Scaling Groups Data
asg_data=$(/usr/local/bin/aws autoscaling describe-auto-scaling-groups --region "$REGION" --profile "$AWS_PROFILE" --output json)

# Ensure AWS CLI returned data
if [ -z "$asg_data" ]; then
    echo "ASG: No Data"
    exit 1
fi

# Extract Relevant Data
total_instances=0
total_healthy=0
total_unhealthy=0
declare -A ASG_OUTPUT

# Parse JSON Data using jq
while IFS=$'\t' read -r asg_name instance_count healthy_count; do
    # Ignore ASGs with 0 instances
    if [[ "$instance_count" -eq 0 ]]; then
        continue
    fi

    unhealthy_count=$((instance_count - healthy_count))

    # Update totals
    total_instances=$((total_instances + instance_count))
    total_healthy=$((total_healthy + healthy_count))
    total_unhealthy=$((total_unhealthy + unhealthy_count))

    # Calculate health percentage for the ASG
    if [[ "$instance_count" -gt 0 ]]; then
        health_percentage=$((100 * healthy_count / instance_count))
    else
        health_percentage=0
    fi

    

    # Determine health icon
    if [[ "$health_percentage" -lt 100 ]]; then
        health_icon="⚠️"
    else
        health_icon="✅"
    fi

    # Store ASG data for manual ordering
    ASG_OUTPUT[$asg_name]="$health_icon $asg_name: $health_percentage% ($instance_count) | href=https://us-east-1.console.aws.amazon.com/ec2/home?region=us-east-1#AutoScalingGroupDetails:id=$asg_name;view=instanceManagement\n-- Healthy: $healthy_count | color=green\n-- Unhealthy: $unhealthy_count | color=red"
done < <(echo "$asg_data" | jq -r '
  .AutoScalingGroups[]
  | select(.Instances | length > 0)
  | [.AutoScalingGroupName, (.Instances | length), (.Instances | map(select(.HealthStatus == "Healthy")) | length)]
  | @tsv')

# Correctly Calculate Overall Health
overall_health=0
if [[ "$total_instances" -gt 0 ]]; then
    overall_health=$((100 * total_healthy / total_instances))
fi

# Determine warning emoji if health is below 100%
health_status_icon=""
if [[ "$overall_health" -lt 100 ]]; then
    health_status_icon="⚠️"
elif [[ "$overall_health" -eq 100 ]]; then
    health_status_icon="✅"
fi

# Display in xbar Format
echo "AWS ASG: $overall_health% ($total_instances) $health_status_icon"
echo "---"

# Track ASGs that have already been displayed
declare -A DISPLAYED_ASGS


# Display ASGs in manual order
for entry in "${ORDERED_ASGS[@]}"; do
    IFS=':' read -r asg_key asg_display <<< "$entry"
    if [[ -n ${ASG_OUTPUT[$asg_key]} ]]; then
        echo -e "${ASG_OUTPUT[$asg_key]/$asg_key/$asg_display}"
        DISPLAYED_ASGS[$asg_key]=1  # Mark this ASG as displayed
    fi
done

# Display remaining ASGs in alphabetical order
for asg_key in ${(k)ASG_OUTPUT}; do
    if [[ -z ${DISPLAYED_ASGS[$asg_key]} ]]; then
        echo -e "${ASG_OUTPUT[$asg_key]}"
    fi
done
