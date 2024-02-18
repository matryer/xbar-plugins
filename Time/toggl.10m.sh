#!/usr/bin/env bash

export PATH='/bin:/usr/local/bin:/usr/bin:/opt/homebrew/bin:$PATH'

#  <xbar.title>Toggl Xbar</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Rob Dyson</xbar.author>
#  <xbar.author.github>rdyson</xbar.author.github>
#  <xbar.desc>Displays current week's total time logged in Toggl for one or more workspaces</xbar.desc>
#  <xbar.image>https://raw.githubusercontent.com/rdyson/toggl-xbar/29f6082807194a95c1cc28d36af4ddf678f5aa88/icon.png</xbar.image>
#  <xbar.dependencies>jq</xbar.dependencies>
#  <xbar.var>string(VAR_WORKSPACES): Workspace ID(s). Separate multiple IDs with a space.</xbar.var>
#  <xbar.var>string(VAR_TOGGL_API_KEY): Toggl API Key.</xbar.var>
#  <xbar.var>select(VAR_WEEK_START_DAY="Sunday"): Week starts on [Monday, Sunday]</xbar.var>
#  <xbar.abouturl>https://github.com/rdyson/toggl-xbar</xbar.abouturl>

# Specifies previous Sunday, can be changed to Monday if you prefer.
start_date=$(date -v -${VAR_WEEK_START_DAY} +"%Y"-"%m"-"%d")

# Loop over all workspaces in workspaces array, summing total seconds.
# Get the "tracked_seconds" value from the curl response and remove brackets and padding.
for workspace in ${VAR_WORKSPACES[@]}
do
  workspace_seconds=$(curl \
    -u ${VAR_TOGGL_API_KEY}:api_token \
    -s \
    -X POST https://api.track.toggl.com/reports/api/v3/workspace/"$workspace"/projects/summary \
    -H "Content-Type: application/json" \
    -d '{"start_date":"'"$start_date"'"}' \
    | jq -c '[.[].tracked_seconds'] | sed 's/\[//' | sed 's/\]//' | sed 's/,/+/g')

  # If workspace_seconds is blank, set to 0.
  if [ ! "$workspace_seconds" ]
  then
   workspace_seconds=0
  fi

  # Add workspace_seconds to total_seconds.
  total_seconds=$(($total_seconds+$workspace_seconds))
done

# Get total seconds as hours, or 0 if no time found.
if [[ $total_seconds == 0 ]]
then
  output="0h";
else
  output=$(awk 'BEGIN { rounded = sprintf("%.0f", "'"$total_seconds"'"/3600); hours = "h"; result = rounded hours; print result }')
fi

# Print final result
echo $output

# Add menu item to go to Toggl website
echo "---"
echo "Toggl Website | href=http://track.toggl.com"

