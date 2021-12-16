#!/usr/local/bin/python
# -*- coding: utf-8 -*-

# <xbar.title>Robinhood</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jonathan Grant</xbar.author>
# <xbar.author.github>jonathangrant</xbar.author.github>
# <xbar.desc>Shows your total portfolio value.</xbar.desc>
# <xbar.dependencies>python,Robinhood</xbar.dependencies>


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
