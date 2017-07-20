#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# <bitbar.title>Poloniex Currency Widget - Widgey</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Peter Stenger</bitbar.author>
# <bitbar.author.github>reteps</bitbar.author.github>
# <bitbar.desc>Gets all wanted currency pairs from poloniex and shows information about it</bitbar.desc>
# <bitbar.image>https://i.imgur.com/fYO0p7U.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>



pairs = {'USDT_BTC':'L','USDT_ETH':'L','USDT_LTC':'L','BTC_STEEM':'S','BTC_STRAT':'S'}

'''add the pair you want to the list.
L, large, 3 decimal places
S, small, 7 decimal places
'''


#---------------CODE----------------------#
try:
    from poloniex import Poloniex
except ModuleNotFoundError:
    print('!!WIDGEY ALERT!!')
    print('---')
    print('Python Poloniex is not installed.')
    print("pip3 install https://github.com/s4w3d0ff/python-poloniex/archive/v0.4.4.zip|href='https://github.com/s4w3d0ff/python-poloniex'")
    exit()
#-----------------------------------#
try:
    p = Poloniex()
    call = p('returnTicker')
    standard = "|href='https://poloniex.com/exchange#{}' font='Menlo'"
    largeformat = "{: <5} {:0<9.3f} {:0<+6.2f}% {:0<9.3f} {:0<9.3f} {:0<9.3f}" + standard
    smallformat = "{: <5} {:0<9.7f} {:0<+6.2f}% {:0<9.7f} {:0<9.7f} {:0<9.7f}" + standard
    #-----------------------------------#
    print('Widgey')
    print('---')
    print('NAME   CURRENT  CHANGE    OPEN      HIGH       LOW   |font=Menlo')
    for pair in pairs.keys():
        #-------------------------------#
        last = float(call[pair]['last'])
        change = float(call[pair]['percentChange'])
        information = [pair.split('_')[1],
        last,
        change * 100,
        float((1 - change) * last),
        float(call[pair]['high24hr']),
        float(call[pair]['low24hr']),
        pair.lower()]
        #--------------------------------#
        if pairs[pair] == 'L':
            print(largeformat.format(*information))
        elif pairs[pair] == 'S':
            print(smallformat.format(*information))
except Exception:
    print('Widgey')
    print('---')
    print('No wifi')
    exit()
