#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>ISS Overhead Check</xbar.title>
# <xbar.version>v0.0.1</xbar.version>
# <xbar.author>Thiago Paes</xbar.author>
# <xbar.author.github>mrprompt</xbar.author.github>
# <xbar.desc>Check International Space Station Pass Times Over !</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

from time import localtime
from requests import get

LATITUDE = -27.591665  # Put your latitude here
LONGITUDE = -48.589599  # Put your longitude here
ALTITUDE = 12  # Put your altitude here


def iss_overhead():
    url = "http://api.open-notify.org/iss-pass.json?lat={}&lon={}&alt={}".format(LATITUDE, LONGITUDE, ALTITUDE)
    response = get(url)
    content = response.json()

    print('🛰')
    separator()

    if content['message'] == 'success':
        for overhead in content['response']:
            ts = overhead['risetime']
            year, month, day, hour, minute, seconds, weekday, year_day, is_dst = localtime(ts)

            print("🛰 {}/{} - {}:{} UTC".format(day, month, hour, minute))
    else:
        print("☠ Invalid response.")


def separator():
    print('---')


# Execution
try:
    iss_overhead()
except Exception as e:
    print('☠ Script error:')
    print(e)
    separator()
