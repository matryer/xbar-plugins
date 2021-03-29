#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Bundesliga Matchday</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alex</xbar.author>
# <xbar.author.github>alexrockt</xbar.author.github>
# <xbar.desc>Shows the next matchday of the Bundesliga</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/alexrockt/misc/master/bundesliga-screenshot.png</xbar.image>
# <xbar.dependencies>Python</xbar.dependencies>
# <xbar.abouturl></xbar.abouturl>

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