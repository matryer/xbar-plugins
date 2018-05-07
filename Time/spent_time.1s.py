#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Spent time displayer</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Gabriel de Maeztu</bitbar.author>
# <bitbar.author.github>merqurio</bitbar.author.github>
# <bitbar.desc>Display the time of the year already gone as percentage</bitbar.desc>
# <bitbar.dependencies>python2l</bitbar.dependencies>

import datetime
from calendar import monthrange, isleap

now = datetime.datetime.now()
day = datetime.datetime(now.year, now.month, now.day)
month = datetime.datetime(now.year, now.month, 1)
year = datetime.datetime(now.year, 1, 1)

day_in_seconds = 24*60*60

if isleap(year.year):
    number_of_days = 366
else:
    number_of_days = 365

spent_day = ((now-day).seconds*100)/day_in_seconds
spent_month = (int((now-month).total_seconds()) *100)/(monthrange(month.year, month.month)[1]*day_in_seconds)
spent_year = (int((now-year).total_seconds())*100)/(number_of_days*day_in_seconds)

def display():
    print('Day → {:d}%'.format(spent_day))
    print('---')
    print('Day → {:d}%'.format(spent_day))
    print('Month → {:d}%'.format(spent_month))
    print('Year → {:d}%'.format(spent_year))

if __name__ == '__main__':
    display()
