#!/usr/bin/python
# <bitbar.title>Stock Ticker</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Robert Kanter</bitbar.author>
# <bitbar.author.github>rkanter</bitbar.author.github>
# <bitbar.desc>Provides a rotating stock ticker in your menu bar, with color and percentage changes</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# Data provided for free by IEX (https://iextrading.com/developer)
import urllib2
import json

# Enter your stock symbols here in the format: ["symbol1", "symbol2", ...]
stock_symbols = ["MSFT", "AAPL", "AMZN", "CLDR"]

response = urllib2.urlopen("https://api.iextrading.com/1.0/stock/market/batch?symbols=" + ','.join(stock_symbols) + "&types=quote&displayPercent=true")
json_data = json.loads(response.read())

for stock_symbol in stock_symbols:
    stock_quote = json_data[stock_symbol]["quote"]
    price_current = stock_quote["latestPrice"]
    price_changed = stock_quote["change"]
    price_percent_changed = stock_quote["changePercent"]

    color = "red" if float(price_changed) < 0 else "green"
    print("{} {:.2f} {:.2f} ({:.2f}%) | color={}".format(stock_symbol, price_current, price_changed, price_percent_changed, color))
