#!/bin/bash

# <bitbar.title>AWS Cost</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Sean Luce</bitbar.author>
# <bitbar.author.github>seanluce</bitbar.author.github>
# <bitbar.desc>Show the current months AWS costs.</bitbar.desc>

#Assumes AWS CLI is installed, 'pip install awscli' or 'pip3 install awscli', and 'aws configure' has been ran.

start=$(date -v1d +%Y-%m-%d)
end=$(date -v+1m -v1d -v-1d +%Y-%m-%d)
export PATH="/usr/local/bin:/usr/bin:$PATH"
prefix=$(aws ce get-cost-and-usage --time-period Start="$start",End="$end" --granularity MONTHLY --metric "BlendedCost" | awk '/Unit/ {print $2;}' | sed 's/[,"]//g')
cost=$(aws ce get-cost-and-usage --time-period Start="$start",End="$end" --granularity MONTHLY --metric "BlendedCost" | awk '/Amount/ {print $2;}' | sed 's/[,"]//g' | xargs printf "%.*f\n" 2)
echo "$cost" "$prefix"