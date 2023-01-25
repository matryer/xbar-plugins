#!/usr/bin/python
#
# <xbar.title>UptimeKuma Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>mariogarridopt</xbar.author>
# <xbar.author.github>mariogarridopt</xbar.author.github>
# <xbar.desc>Show UptimeKuma status</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/mariogarridopt/xBar-Uptime-Kuma</xbar.abouturl>
# <xbar.image>https://raw.githubusercontent.com/mariogarridopt/xBar-Uptime-Kuma/master/screenshot.png</xbar.image>
#

import os
from uptime_kuma_api import UptimeKumaApi

# -------------------------------------------

BASE_URL = 'http://localhost:3001' # no trailing slash
USERNAME = 'admin'
PASSWORD = 'admin'

# -------------------------------------------

try:
    api = UptimeKumaApi(BASE_URL)
    api.login(USERNAME, PASSWORD)

    monitor_list = api.get_monitors()
    monitor_total = len(monitor_list);
    monitor_online = 0;
    monitor_list_string = [];

    ## GET DATA
    for monitor in monitor_list:
        beats = api.get_monitor_beats(monitor['id'], 24)
        text = monitor['name'] + " | href='" + BASE_URL + "/dashboard/" + str(monitor['id']) + "'"
        
        if(beats[-1]['status'] == True):
            monitor_online = monitor_online + 1
            text = "🟢 " + text + " color='green'"
        else:
            text = "❗️ " + text + " color='red'"
        monitor_list_string.append(text);

    ## PRINT DATA
    print(monitor_online, '/', monitor_total)
    print('---')
    for monitors_text in monitor_list_string:
        print(monitors_text)
    print('---')
    print("Open UptimeKuma | href='" + BASE_URL + "/dashboard'")
except:
    print('📛')
    print('---')
    print("No monitor! | color='red'")
    print("Click here to configure | href='file://%s'" % os.path.abspath('uptimekuma.30s.py'))
finally:
    api.disconnect()