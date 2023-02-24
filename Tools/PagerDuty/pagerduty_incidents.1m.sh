#!/usr/bin/env bash

set -euo pipefail

# <xbar.title>Pagerduty Incidents</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Seren Thompson</xbar.author>
# <xbar.author.github>seren</xbar.author.github>
# <xbar.desc>Displays the number of triggered Pagerduty.com incidents assigned to a user</xbar.desc>
# <xbar.image>https://imgur.com/LJR06et.png</xbar.image>
# <xbar.dependencies>bash,jq</xbar.dependencies>
# <xbar.abouturl>https://github.com/seren/xbar-plugins/</xbar.abouturl>

# Variables become preferences in the app:
#
#  <xbar.var>string(USERID=""): Pagerduty user ID (ex. PURY8ZT). Can be found in your Pagerduty user profile URL.</xbar.var>
#  <xbar.var>string(USERTOKEN=""): REST API user token. See https://support.pagerduty.com/docs/generating-api-keys#generating-a-personal-rest-api-key</xbar.var>
#  <xbar.var>string(MAXINCIDENTS="10"): Maximum number of incidents to display</xbar.var>
#  <xbar.var>string(PDHOSTNAME=""): Your organization's unique Pagerduty hostname (ex. microsoft.pagerduty.com)</xbar.var>

pdurl="https://${PDHOSTNAME}/incidents?assignedToUser=${USERID}&status=triggered"

output="$(curl --request GET \
  --url "https://api.pagerduty.com/incidents?limit=${MAXINCIDENTS}&total=false&user_ids%5B%5D=${USERID}&time_zone=UTC&statuses%5B%5D=triggered" \
  --header 'accept: application/vnd.pagerduty+json;version=2' \
  --header "authorization: Token token=${USERTOKEN}" \
  --header 'content-type: application/json' \
  --max-time 10 -s)"

total_high="$(echo "$output" | /usr/local/bin/jq '[.incidents[] | select(.urgency=="high")] | length')"
total_low="$(echo "$output" | /usr/local/bin/jq '[.incidents[] | select(.urgency=="low")] | length')"

# Text to show in the menu bar
if [ "$total_high" != "0" ]; then
  echo "PD_HIGH:${total_high} | color=red href=${pdurl}"
elif [ "$total_low" != "0" ]; then
  echo "pd_low:${total_low} | href=${pdurl}"
else
  echo "pd_ok | href=https://${PDHOSTNAME}/incidents"
fi

# Text to show in the xbar menu

if [ "$total_high" != "0" ]; then
  echo "---"
  # The sed command strips out extra verbage from Nagios-generated incidents
  echo "High urgency: | color=red href=${pdurl}"
  # shellcheck disable=2016
  echo "$output" | /usr/local/bin/jq '.incidents[] | select(.urgency=="high") | .title' | sed 's/^"\(SERVICEDESC=\)*\([^;]*\)\(.*\)"/\2/' | sed "s#\$# | href=${pdurl}#"
fi

if [ "$total_low" != "0" ]; then
  echo "---"
  # The sed command strips out extra verbage from Nagios-generated incidents
  echo "Low urgency: | color=blue href=${pdurl}"
  # shellcheck disable=2016
  echo "$output" | /usr/local/bin/jq '.incidents[] | select(.urgency=="low") | .title' | sed 's/^"\(SERVICEDESC=\)*\([^;]*\)\(.*\)"/\2/' | sed "s#\$# | href=${pdurl}#"
fi

exit 0
