#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Todoist Today</xbar.title>
# <xbar.version>v2.1.0</xbar.version>
# <xbar.author>K.Kobayashi, et al</xbar.author>
# <xbar.author.github>kobayashi,gingerbeardman</xbar.author.github>
# <xbar.desc>Today's task in your menu bar!</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>http://i.imgur.com/f37VtAg.png</xbar.image>

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

