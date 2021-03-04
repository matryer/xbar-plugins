#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Beeminder</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ben Congdon</bitbar.author>
# <bitbar.author.github>bcongdon</bitbar.author.github>
# <bitbar.image>https://i.imgur.com/XamwU1E.png</bitbar.image>
# <bitbar.desc>Displays your active Beeminder goals and their due dates/amounts</bitbar.desc>
# <bitbar.dependencies>python,requests</bitbar.dependencies>

import requests

# NOTE: Change these to set your credentials
USERNAME = ''
AUTH_TOKEN = ''

# Don't change anythine below this line
if not USERNAME or not AUTH_TOKEN:
    print('⚠️\n---\nBeeminder: No username and/or auth token provided!')
    exit(1)
API_URL = 'https://www.beeminder.com/api/v1/users/{}.json'.format(USERNAME)

req = requests.get(API_URL, params=dict(
    auth_token=AUTH_TOKEN, datapoints_count=1, associations=True))

data = req.json()
goals = data['goals']

output = '🐝\n---\n'
max_slug_len = max(len(goal['slug']) + 1 for goal in goals)
for goal in goals:
    if goal.get('coasting'):
        continue
    goal_url = 'https://www.beeminder.com/{}/{}'.format(USERNAME, goal['slug'])
    goal_params = 'href={} color={}'.format(
        goal_url, goal['roadstatuscolor'])
    output += "{:<20.15s}\t{}|{}\n".format(goal['slug'] + ':',
                                           goal['limsum'], goal_params)

print(output)
