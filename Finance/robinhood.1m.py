#!/usr/local/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Robinhood</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jonathan Grant</bitbar.author>
# <bitbar.author.github>jonathangrant</bitbar.author.github>
# <bitbar.desc>Shows your total portfolio value.</bitbar.desc>
# <bitbar.dependencies>python,Robinhood</bitbar.dependencies>


from Robinhood import Robinhood as R

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
print('${:,.2f}|color={}'.format(float(now), color))
print('---')
print('Start of Day: ${:,.2f}|color={}'.format(float(start), 'white'))
print('Difference: ${:,.2f}|color={}'.format(float(difference), color))
