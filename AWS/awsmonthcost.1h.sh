#!/bin/bash

# <xbar.title>AWS Cost</xbar.title>
# <xbar.version>v1.2.0</xbar.version>
# <xbar.author>Sean Luce</xbar.author>
# <xbar.desc>Show the current months AWS costs.</xbar.desc>

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:$PATH"

start=$(date -v1d +%Y-%m-%d)
end=$(date -v+1m -v1d -v-1d +%Y-%m-%d)

result=$(aws ce get-cost-and-usage --time-period Start="$start",End="$end" --granularity MONTHLY --metric "BlendedCost" 2>&1)

# Check if AWS CLI is installed
if ! command -v aws &> /dev/null; then
    echo "AWS: !"
    echo "---"
    echo "⚠️ AWS CLI not found | color=red"
    echo "Install: brew install awscli | color=gray size=11"
    exit 0
fi

# Check for permission errors
if echo "$result" | grep -q "AccessDeniedException"; then
    echo "AWS: --"
    echo "---"
    echo "⚠️ No billing permission | color=red"
    echo "Add ce:GetCostAndUsage to IAM user | color=gray size=11"
    exit 0
fi

# Check for credential errors
if echo "$result" | grep -q "InvalidUserID.NotFound\|UnauthorizedOperation\|SignatureDoesNotMatch\|InvalidAccessKeyId\|TokenRefreshRequired"; then
    echo "AWS: ⚠️"
    echo "---"
    echo "⚠️ Invalid AWS credentials | color=red"
    echo "Run: aws configure | color=gray size=11"
    exit 0
fi

# Check for network/connectivity errors
if echo "$result" | grep -q "Could not connect to the endpoint URL\|Connection timed out\|Name or service not known"; then
    echo "AWS: ⚠️"
    echo "---"
    echo "⚠️ Network connection error | color=red"
    echo "Check internet connection | color=gray size=11"
    exit 0
fi

# Check for region/service errors
if echo "$result" | grep -q "InvalidRegion\|OptInRequired"; then
    echo "AWS: ⚠️"
    echo "---"
    echo "⚠️ Region or service error | color=red"
    echo "Check AWS region configuration | color=gray size=11"
    exit 0
fi

# Check for other AWS errors
if echo "$result" | grep -q "An error occurred"; then
    echo "AWS: ?"
    echo "---"
    echo "⚠️ AWS API error | color=red"
    echo "Check AWS CLI configuration | color=gray size=11"
    exit 0
fi

prefix=$(echo "$result" | awk '/Unit/ {print $2;}' | sed 's/[,"]//g')
cost=$(echo "$result" | awk '/Amount/ {print $2;}' | sed 's/[,"]//g' | xargs printf "%.*f\n" 2)

if [ -z "$cost" ]; then
    echo "AWS: ?"
else
    echo "$cost $prefix"
fi
