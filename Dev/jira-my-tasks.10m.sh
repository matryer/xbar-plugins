#!/usr/bin/env bash

# <xbar.title>Jira My Tasks</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Luigi Lotito</xbar.author>
# <xbar.author.github>lglot</xbar.author.github>
# <xbar.desc>Shows your assigned Jira tickets grouped by project and status. Click any ticket to open it in the browser.</xbar.desc>
# <xbar.dependencies>python3, curl</xbar.dependencies>
# <xbar.abouturl>https://github.com/lglot</xbar.abouturl>
#
# <xbar.var>string(JIRA_URL=""): Your Jira instance URL (e.g. https://yourcompany.atlassian.net)</xbar.var>
# <xbar.var>string(JIRA_EMAIL=""): Your Jira account email</xbar.var>
# <xbar.var>string(JIRA_API_TOKEN=""): Jira API token (create one at https://id.atlassian.com/manage-profile/security/api-tokens)</xbar.var>
# <xbar.var>string(JIRA_JQL="assignee=currentUser() AND statusCategory != Done ORDER BY project,status"): JQL query to filter tickets</xbar.var>
# <xbar.var>number(MAX_RESULTS=50): Maximum number of tickets to fetch</xbar.var>

JIRA_URL="${JIRA_URL%/}"
JQL="${JIRA_JQL:-assignee=currentUser() AND statusCategory != Done ORDER BY project,status}"
MAX="${MAX_RESULTS:-50}"

if [ -z "$JIRA_URL" ] || [ -z "$JIRA_EMAIL" ] || [ -z "$JIRA_API_TOKEN" ]; then
  echo "⚠️ JIRA"
  echo "---"
  echo "Plugin not configured | color=red"
  echo "Set JIRA_URL, JIRA_EMAIL and JIRA_API_TOKEN | color=gray"
  echo "Open SwiftBar plugin settings to configure | color=gray"
  exit 0
fi

ENCODED_JQL=$(python3 -c "import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1]))" "$JQL")

RESPONSE=$(curl -s --max-time 10 -u "${JIRA_EMAIL}:${JIRA_API_TOKEN}" \
  -H "Content-Type: application/json" \
  "${JIRA_URL}/rest/api/3/search/jql?jql=${ENCODED_JQL}&fields=summary,status,key,project&maxResults=${MAX}" 2>/dev/null)

if [ -z "$RESPONSE" ]; then
  echo "⚠️ JIRA"
  echo "---"
  echo "Request failed (timeout or network error) | color=red"
  echo "Refresh | refresh=true"
  exit 0
fi

python3 -c "
import sys, json
from collections import OrderedDict

try:
    data = json.loads(sys.argv[1])
except (json.JSONDecodeError, IndexError):
    print('⚠️ JIRA')
    print('---')
    print('Invalid response from Jira | color=red')
    sys.exit(0)

if 'issues' not in data:
    print('⚠️ JIRA')
    print('---')
    msg = data.get('errorMessages', ['Unknown error'])[0] if 'errorMessages' in data else 'Auth failed or bad request'
    print(f'{msg} | color=red')
    sys.exit(0)

issues = data['issues']
total = len(issues)
base = sys.argv[2]

if total == 0:
    print('✅ JIRA')
    print('---')
    print('No open tickets assigned to you')
    print('---')
    print(f'Open Jira | href={base}')
    print('Refresh | refresh=true')
    sys.exit(0)

# Group by project, then by status
projects = OrderedDict()
for issue in issues:
    proj_key = issue['fields']['project']['key']
    proj_name = issue['fields']['project']['name']
    status_name = issue['fields']['status']['name']
    status_cat = issue['fields']['status']['statusCategory']['key']
    projects.setdefault((proj_key, proj_name), OrderedDict())
    projects[(proj_key, proj_name)].setdefault((status_name, status_cat), []).append(issue)

# Emoji based on statusCategory.key (always English, regardless of Jira locale)
cat_emoji = {
    'new':           '⚪',  # To Do
    'indeterminate': '🔵',  # In Progress
    'done':          '✅',  # Done
    'undefined':     '⚫',
}

print(f'📋 {total}')
print('---')

for (proj_key, proj_name), statuses in projects.items():
    proj_count = sum(len(v) for v in statuses.values())
    print(f'{proj_key} — {proj_name} ({proj_count})')
    for (status_name, status_cat), items in statuses.items():
        e = cat_emoji.get(status_cat, '⚫')
        print(f'--{e} {status_name} ({len(items)})')
        for i in items:
            key = i['key']
            summary = i['fields']['summary'][:55]
            url = f'{base}/browse/{key}'
            print(f'--{key}: {summary} | href={url}')
    print('---')

print(f'Open Jira | href={base}')
print('Refresh | refresh=true')
" "$RESPONSE" "$JIRA_URL"
