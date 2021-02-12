#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

# <bitbar.title>Robinhood2</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Adam Jilling</bitbar.author>
# <bitbar.author.github>ajilling</bitbar.author.github>
# <bitbar.desc>Shows your total portfolio value and daily change</bitbar.desc>
# <bitbar.image>https://i.ibb.co/SBY7NXy/bitbar-robinhood.png</bitbar.image>
# <bitbar.dependencies>python,robin_stocks</bitbar.dependencies>

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
