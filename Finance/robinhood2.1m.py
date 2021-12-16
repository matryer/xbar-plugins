#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

# <xbar.title>Robinhood2</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Adam Jilling</xbar.author>
# <xbar.author.github>ajilling</xbar.author.github>
# <xbar.desc>Shows your total portfolio value and daily change</xbar.desc>
# <xbar.image>https://i.ibb.co/SBY7NXy/bitbar-robinhood.png</xbar.image>
# <xbar.dependencies>python,robin_stocks</xbar.dependencies>

import robin_stocks as rh

login = rh.login('<username>', '<password>')

portfolio = rh.profiles.load_portfolio_profile()
current_value = portfolio['equity']
open_value = portfolio['equity_previous_close']

change = float(current_value) - float(open_value)

# TODO: colors not working in Big Sur
color = 'white'
move = ''
if (change > 0):
    color = 'green'
    move = '▲'
elif (change < 0):
    color = 'red'
    move = '▼'

output1 = '{:,.2f}'.format(float(current_value))
output2 = '  ' + move
output3 = '{:,.2f}'.format(abs(float(change)))
print(output1 + output2 + output3)
