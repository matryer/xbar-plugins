#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Todoist Today</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>K.Kobayashi</bitbar.author>
# <bitbar.author.github>chiisaihayashi</bitbar.author.github>
# <bitbar.desc>Today's task in your menu bar!</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/f37VtAg.png</bitbar.image>

import urllib, urllib2
import json
import datetime

api_key = ''
url = 'https://todoist.com/API/v7/sync'
value = { 'token': api_key, 'resource_types': '["all"]', 'seq_no': 0 }
data = urllib.urlencode(value)

d = datetime.datetime.today()
today = str(d.day)+d.strftime(" %b")
today_y = str(d.day)+d.strftime(" %b ")+str(d.year)
comment = "Today's task: "

try:
    request = urllib2.Request(url, data)
    r = urllib2.urlopen(request)
    j = json.loads(r.read())
    items = j['items']
    for item in items:
        due = item['date_string'] # due date of a task
        if (due == today) or (due == today_y):
            print(comment + item['content'].encode("utf-8"))
finally:
    r.close()
