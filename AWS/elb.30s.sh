#!/bin/bash

# Percentage of healthy EC2 instances behind an ELB
#   Dropdown with healthy and unhealthy totals
#
# by Jonathan Keith (http://github.com/joncse)
#
# <bitbar.title>AWS ELB Healthy Percentage</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonathan Keith</bitbar.author>
# <bitbar.author.github>joncse</bitbar.author.github>
# <bitbar.desc>Shows the percentage of healthy EC2 instances behind an ELB along with a dropdown to display the healthy and unhealthy totals.</bitbar.desc>
# <bitbar.dependencies>awscli</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/nQ6LzvZ.png</bitbar.image>
#
# Dependencies: 
#   awscli (https://aws.amazon.com/cli/)

export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"

## Required Configuration (must provide your own settings here)

# The name of the Elastic Load Balancer
LOAD_BALANCER=""

# AWS CLI credential profile
AWS_CLI_PROFILE="default"

## Optional Configuration (not required)

# Prefix label
MENU_BAR_PREFIX_LABEL="ELB: "

# InService output color (default green)
IN_SERVICE_COLOR="#29cc00"

# OutOfService output color (default red)
OUT_SERVICE_COLOR="#ff0033"

## Implementation (changes optional, not required)

if [ -z "$LOAD_BALANCER" ]; then
  echo "Missing configuration: load balancer name"
  exit 1
fi

# Fetch list of instance health statuses (InService or OutOfService)
status=$(aws --profile $AWS_CLI_PROFILE elb describe-instance-health --load-balancer-name $LOAD_BALANCER --query 'InstanceStates[*].[State]' --output text)

# Total number of lines fetched
total=$(echo "$status" | tr ' ' '\n' | wc -l | xargs)

# Number of lines containing "In" (matches InService lines)
in=$(echo "$status" | tr ' ' '\n' | grep -c In | xargs)

# Number of lines containing "Out" (matches OutOfService lines)
out=$(echo "$status" | tr ' ' '\n' | grep -c Out | xargs)

# Percentage calculation
percent=$(bc -l <<< "($in / $total) * 100" | xargs printf "%1.0f")

# Output
echo "$MENU_BAR_PREFIX_LABEL $percent%"
echo "---"
echo "In: $in | color=$IN_SERVICE_COLOR"
echo "Out: $out | color=$OUT_SERVICE_COLOR"
