#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# -*- coding: utf-8 -*-
# <bitbar.title>Bundesliga Matchday</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Alex</bitbar.author>
# <bitbar.author.github>alexrockt</bitbar.author.github>
# <bitbar.desc>Shows the next matchday of the Bundesliga</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/alexrockt/misc/master/bundesliga-screenshot.png</bitbar.image>
# <bitbar.dependencies>Python</bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

import requests
import datetime

URL = "https://www.openligadb.de/api/getmatchdata/bl1"

result = requests.get(URL)
data = result.json()

matchday = data[0]['Group']['GroupName']

date_time = ""

print('{} | size=10 color=green'.format(matchday))
print('---')
for game in data:
    if date_time != game['MatchDateTime']:
        date_time = game['MatchDateTime']
        print('---')
        date_time_fmt = datetime.datetime.strptime(date_time,"%Y-%m-%dT%H:%M:%S")
        date_time_fmt = datetime.datetime.strftime(date_time_fmt, "%H:%M / %d.%m.%Y")
        print("{} | color=red".format(date_time_fmt))
    date_time = game['MatchDateTime']
    
    team1 = game['Team1']['TeamName']
    team2 = game['Team2']['TeamName']
    
    print("{} - {} | color=white".format(team1, team2))