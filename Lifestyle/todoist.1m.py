#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Todoist Today</bitbar.title>
# <bitbar.version>v2.1.0</bitbar.version>
# <bitbar.author>K.Kobayashi, et al</bitbar.author>
# <bitbar.author.github>kobayashi,gingerbeardman</bitbar.author.github>
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
url = 'https://api.todoist.com/sync/v8/sync'
value = { 'token': api_key, 'sync_token': '*', 'resource_types': '["all"]' }
data = urlencode(value).encode('utf-8')

d = datetime.datetime.today()
today = d.strftime("%Y-%m-%d")
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
            if (item['due']): 
                due = item['due']['date'] # due date of a task
                if (due.startswith(today)):
                    if ("T" in due): 
                        time = "@%s " % due[-8:-3]
                    else:
                        time = ""
                    print((comment + time + item['content']).encode('utf-8'))
    finally:
        r.close()

