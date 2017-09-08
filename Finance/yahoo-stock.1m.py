#!/usr/bin/python
# <bitbar.title>Yahoo Finance Stock Ticker</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>David Walker</bitbar.author>
# <bitbar.author.github>bp1222</bitbar.author.github>
# <bitbar.desc>Provides a rotating stock ticker in your menu bar, with color and percentage changes</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
import urllib2

stocks = ["IBM", "UPS", "MSFT", "AAPL", "AMZN"]
url = 'http://download.finance.yahoo.com/d/quotes.csv?s={}&f=spl1'.format(
    ",".join(stocks))

u = urllib2.urlopen(url)
query = u.read().decode('utf-8')

rows = query.strip().split("\n")
for row in rows:
    data = row.replace('"', '').split(',')
    ticker = data[0]
    opened = round(float(data[1]), 2)
    current = round(float(data[2]), 2)
    change = round(current - opened, 2)
    pct = round((abs(change) / ((opened + current) / 2)) * 100, 2)

    color = "red" if change < 0 else "green"

    print("{} ${:,.2f} {:+.2f} ({:+.2f}%) | color={}".format(
        ticker, current, change, pct, color
    ))
