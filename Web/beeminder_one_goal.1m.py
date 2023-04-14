#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" python3
# -*- coding: utf-8 -*-

# Metadata allows the plugin to show up in the xbar app and website.
#
#  <xbar.title>Beeminder</xbar.title>
#  <xbar.version>v0.1</xbar.version>
#  <xbar.author>Tom Adamczewski</xbar.author>
#  <xbar.author.github>tadamcz</xbar.author.github>
#  <xbar.desc>Concisely show the status of a Beeminder goal</xbar.desc>
#  <xbar.image>https://images2.imgbox.com/8d/2d/wLOcD9sz_o.png</xbar.image>
#  <xbar.dependencies>python</xbar.dependencies>

# <xbar.var>string(VAR_AUTH_TOKEN=None): Your Beeminder auth token</xbar.var>
# <xbar.var>string(VAR_GOAL_SLUG='exercise'): Your Beeminder goal slug e.g. 'exercise' for beeminder.com/myusername/exercise</xbar.var>
# <xbar.var>string(VAR_GOAL_EMOJI='🏋️'): Emoji to represent your goal (will appear in the menu bar)</xbar.var>

import os
import json
import time
from urllib.request import urlopen, Request
from urllib.parse import urlencode
from urllib.error import URLError

# Script-level variables
AUTH_TOKEN = os.environ["VAR_AUTH_TOKEN"]
GOAL_SLUG = os.environ["VAR_GOAL_SLUG"]
GOAL_EMOJI = os.environ["VAR_GOAL_EMOJI"]

# Check variables
if not AUTH_TOKEN:
    raise Exception("Please set your Beeminder auth token in the script variables")
if not GOAL_SLUG:
    raise Exception("Please set your Beeminder goal slug in the script variables")
if not GOAL_EMOJI:
    raise Exception("Please set your goal emoji in the script variables")


# Get data

# Docs: "Since appending an access_token to the request uniquely identifies a user, you can alternatively make the request to /users/me.json (without the username)."
API_URL = f'https://www.beeminder.com/api/v1/users/me.json'
params = dict(auth_token=AUTH_TOKEN, datapoints_count=1, associations=True)
url_with_params = f'{API_URL}?{urlencode(params)}'


retries = 13
backoff_factor = 0.1
data = None

for i in range(retries):
    try:
        response = urlopen(url_with_params)
        if response.status != 200:
            raise URLError(f'Error fetching data: status {response.status}')
        data = json.loads(response.read())
        goals = data['goals']
        resolved_username = data['username']
        break
    except URLError as e:
        if i < retries - 1:
            time.sleep(backoff_factor * (2 ** i))
            continue
        else:
            raise Exception(f'Error fetching data: {e}')

# Select goal
goal_emoji = GOAL_EMOJI
goal_slug = GOAL_SLUG
chinups = next(filter(lambda goal: goal['slug'] == goal_slug, goals))
goal = chinups

# Assemble output
color_emojis = {
    "green": "🟢",
    "blue": "🔵",
    "orange": "🟠",
    "red": "🔴",
}

goal_url = f'https://www.beeminder.com/{resolved_username}/{goal["slug"]}'
color_emoji = color_emojis[goal["roadstatuscolor"]]
message = goal['limsumdays']

# Abbreviate message to take up less space in menu bar
message = message.replace("due ", "").replace("days", "d").replace("day", "d")

output = []
output.append(f"{goal_emoji}{color_emoji}{message}")  # Shown in menu bar
output.append("---")
output.append(f"{goal_url} | href={goal_url}")  # Shown in dropdown
print("\n".join(output))
