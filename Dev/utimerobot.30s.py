#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>UptimeRobot Monitor</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Rodrigo Brito</bitbar.author>
# <bitbar.author.github>rodrigo-brito</bitbar.author.github>
# <bitbar.desc>Show UptimeRobot status</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/rodrigobrito/uptimerobot-bitbar</bitbar.abouturl>
#
# by Rodrigo Brito

import requests
import json
import sys
import os

reload(sys)
sys.setdefaultencoding('utf8')

# Set your API Key here!
# Get your API Key in https://uptimerobot.com/dashboard.php#mySettings
api_key = 'CHANGE_HERE!!'

url = 'https://api.uptimerobot.com/v2/getMonitors'

status = {
    0: {'text': 'PAUSED', 'color': 'white'},
    1: {'text': 'NOT CHECKED', 'color': 'white'},
    2: {'text': 'UP', 'color': 'green'},
    8: {'text': 'SEEMS DOWN', 'color': 'yellow'},
    9: {'text': 'DOWN', 'color': 'red'}
}

payload = "api_key=%s&format=json&logs=0" % api_key
headers = {
    'content-type': "application/x-www-form-urlencoded",
    'cache-control': "no-cache"
}

# Request uptime robot API
response = requests.request('POST', url, data=payload, headers=headers)
data = json.loads(response.text)

# Get monitors status count (up, total)
def status_count(monitors):
    up = 0
    for monitor in monitors:
        if monitor.get('status') <= 2:
            up += 1
    return up, len(monitors)

if data.get('monitors'):
    print "%d / %d" % (status_count(data.get('monitors')))
    print "---"
    for monitor in data.get('monitors'):
        st = status.get(monitor.get('status'))
        print  "%s - %s | href='%s' color='%s'" % (st.get('text'),
        monitor.get('friendly_name').encode('utf-8'), monitor.get('url'), st.get('color'))
else:
    print "No monitor! | color='red'"
    print "Click here to configure | href='file://%s'" % os.path.abspath('uptimerobot.30s.py')

print "---"
print "Open UptimeRobot | href='https://uptimerobot.com/dashboard'" 