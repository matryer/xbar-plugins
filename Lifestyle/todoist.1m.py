#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Todoist Today</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>K.Kobayashi</bitbar.author>
# <bitbar.author.github>chiisaihayashi</bitbar.author.github>
# <bitbar.desc>Today's task in your menu bar!</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/zXbovPP.png</bitbar.image>

import urllib, urllib2
import json
import datetime

api_key = ''
url = 'https://todoist.com/API/v6/sync'
value = { 'token': api_key, 'resource_types': '["all"]', 'seq_no': 0 }
data = urllib.urlencode(value)

d = datetime.datetime.today()
today = str(d.day)+d.strftime(" %b")
comment = "Today's task: "

try:
    request = urllib2.Request(url, data)
    r = urllib2.urlopen(request)
    j = json.loads(r.read())
    items = j['Items']
    for item in items:
      if item['due_date'] != None:
        due_item = item['due_date'].split()
        due_day = due_item[1] + " " + due_item[2]
        if due_day == today:
          print(comment + item['content'])

finally:
    r.close()
