#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Todoist Today</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>K.Kobayashi</bitbar.author>
# <bitbar.author.github>kobayashi</bitbar.author.github>
# <bitbar.desc>Today's task in your menu bar!</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/f37VtAg.png</bitbar.image>

import sys
if sys.version_info[0] < 3:
    from urllib2 import urlopen, Request
    from urllib import urlencode
else:
    from urllib.request import urlopen, Request
    from urllib.parse import urlencode
import json
import datetime

api_key = ''
url = 'https://todoist.com/API/v7/sync'
value = { 'token': api_key, 'resource_types': '["all"]', 'seq_no': 0 }
data = urlencode(value).encode('utf-8')

d = datetime.datetime.today()
today = str(d.day)+d.strftime(" %b")
today_y = str(d.day)+d.strftime(" %b ")+str(d.year)
comment = "Today's task: "

if len(api_key) == 0:
    print("set api first")
else:
    try:
        request = Request(url, data)
        r = urlopen(request)
        j = json.loads(r.read())
        items = j['items']
        for item in items:
            due = item['date_string'] # due date of a task
            if (due == today) or (due == today_y):
                print((comment + item['content']).encode('utf-8'))
    finally:
        r.close()

