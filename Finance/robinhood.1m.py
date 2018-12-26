#!/usr/local/bin/python
# -*- coding: utf-8 -*-

from Robinhood import Robinhood as R
from pprint import pprint

me = R()
me.login(username="Your Username Here", password="Your Password Here")
data = me.portfolios()
start = data.get('equity_previous_close', '0')
now = data.get('equity', '0')
color = 'white'
difference = float(now) - float(start)
if difference > 0:
    color = 'green'
elif difference < 0:
    color = 'red'
print('${}|color={}'.format(now, color))
