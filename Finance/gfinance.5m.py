#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>Google Finance Stock Ticker</bitbar.title>
# <bitbar.version>1.4</bitbar.version>
# <bitbar.author>Jamieson Colburn</bitbar.author>
# <bitbar.author.github>jamiesonio</bitbar.author.github>
# <bitbar.desc>Provides a rotating stock ticker in your menu bar</bitbar.desc>
# <bitbar.image>https://s3.amazonaws.com/jamieson.io/bitbar_gfinance.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
import urllib2

stocks={"MSFT","AAPL","GOOGL","AMZN","ONDK"}

stocks = sorted(stocks)
url = "http://download.finance.yahoo.com/d/quotes.csv?s={}&f=l1c1".format(','.join(stocks))
u = urllib2.urlopen(url)
response = u.read().strip()
for i, csv in enumerate(response.split('\n')):
    symbol = stocks[i]
    price, change = csv.split(',')
    try:
        price = float(price)
        change = float(change)
        direction = "+" if change > 0 else ""
        color = "red" if change < 0 else "green"
    except ValueError:
        change = 0
        direction = ""
        color = "gray"
    print("{} {} {}{} | color={}".format(symbol, price, direction, change, color))
