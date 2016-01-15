#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>Google Finance Stock Ticker</bitbar.title>
# <bitbar.version>1.4</bitbar.version>
# <bitbar.author>Jamieson Colburn</bitbar.author>
# <bitbar.author.github>jamiesonio</bitbar.author.github>
# <bitbar.desc>Provides a rotating stock ticker in your menu bar</bitbar.desc>
# <bitbar.image>http://nothingreally.botler.me/bitbar.currency-tracker.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
import urllib2
import json
import time

#Stocks can be provided with just the symbol (AAPL) or exchange:symbol (NASDAQ:AAPL)
stocks={"MSFT","AAPL","GOOGL","AMZN","ONDK"}

query = ""
for i in stocks:
    query = i + "," + query

url = "http://finance.google.com/finance/info?client=ig&q=" + query
u = urllib2.urlopen(url)
query = u.read()
obj = json.loads(query[4:-1])

for ticker in obj:
    if float(ticker["c"]) < 0:
        print ticker["t"], ticker["l"], ticker["c"], "| color=red"
    elif float(ticker["c"]) > 0:
        print ticker["t"], ticker["l"], ticker["c"], "| color=green"