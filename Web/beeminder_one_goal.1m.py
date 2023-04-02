#! ../python_env/bin/python
# -*- coding: utf-8 -*-

# Metadata allows the plugin to show up in the xbar app and website.
#
#  <xbar.title>Beeminder</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>Tom Adamczewski</xbar.author>
#  <xbar.author.github>tadamcz</xbar.author.github>
#  <xbar.desc>Concisely show the status of a Beeminder goal</xbar.desc>
#  <xbar.image>https://images2.imgbox.com/8d/2d/wLOcD9sz_o.png</xbar.image>
#  <xbar.dependencies>python,python requests library</xbar.dependencies>

# <xbar.var>string(VAR_USERNAME='johnsmith'): Your Beeminder username</xbar.var>
# <xbar.var>string(VAR_AUTH_TOKEN=''): Your Beeminder auth token</xbar.var>
# <xbar.var>string(VAR_GOAL_SLUG='exercise'): Your Beeminder goal slug e.g. 'exercise' for beeminder.com/myusername/exercise</xbar.var>
# <xbar.var>string(VAR_GOAL_EMOJI='🏋️'): Emoji to represent your goal (will appear in the menu bar)</xbar.var>

import requests
from requests.adapters import HTTPAdapter, Retry
import os

# Script-level variables
USERNAME = os.environ["VAR_USERNAME"]
AUTH_TOKEN = os.environ["VAR_AUTH_TOKEN"]
GOAL_SLUG = os.environ["VAR_GOAL_SLUG"]
GOAL_EMOJI = os.environ["VAR_GOAL_EMOJI"]

# Set up Session with retries
retries = Retry(total=10, backoff_factor=0.1)
s = requests.Session()
s.mount('https://', HTTPAdapter(max_retries=retries))

# Get data
if not USERNAME or not AUTH_TOKEN:
    print('⚠️\n---\nBeeminder: No username and/or auth token provided!')
    exit(1)
API_URL = 'https://www.beeminder.com/api/v1/users/{}.json'.format(USERNAME)


req = s.get(API_URL, params=dict(
    auth_token=AUTH_TOKEN, datapoints_count=1, associations=True))

data = req.json()
goals = data['goals']

# Select goal
goal_emoji = GOAL_EMOJI
goal_slug = GOAL_SLUG
chinups = next(filter(lambda goal: goal['slug'] == goal_slug, goals))
goal = chinups

# Assemble output
color_emojis = {
    "green":"🟢",
    "blue":"🔵",
    "orange": "🟠",
    "red":"🔴",
}

goal_url = 'https://www.beeminder.com/{}/{}'.format(USERNAME, goal['slug'])
color_emoji = color_emojis[goal["roadstatuscolor"]]
message = goal['limsumdays']

# Abbreviate message to take up less space in menu bar
message = message.replace("due ", "").replace("days","d").replace("day","d")

output = []
output.append(f"{goal_emoji}{color_emoji}{message}") # Shown in menu bar
output.append("---")
output.append(f"{goal_url} | href={goal_url}") # Shown in dropdown
print("\n".join(output))
