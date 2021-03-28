#!/usr/bin/python
# <xbar.title>Stock Ticker</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Robert Kanter</xbar.author>
# <xbar.author.github>rkanter</xbar.author.github>
# <xbar.desc>Provides a rotating stock ticker in your menu bar, with color and percentage changes</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>https://i.imgur.com/Nf4jiRd.png</xbar.image>
# <xbar.abouturl>https://github.com/rkanter</xbar.abouturl>
import urllib2
import json

#-----------------------------------------------------------------------------
# IMPORTANT: You will need an API Token.  Follow these steps
# 1. Create a free account at https://iexcloud.io/cloud-login#/register/
# 2. Select the free "START" tier
# 3. Verify your email address
# 4. Click "API Tokens" in the left menu
# 5. Enter the "Publishable" Token in the quotes below (it should start with "pk_")
api_token = ""

# Enter your stock symbols here in the format: ["symbol1", "symbol2", ...]
stock_symbols = ["MSFT", "AAPL", "AMZN"]
#-----------------------------------------------------------------------------

response = urllib2.urlopen("https://cloud.iexapis.com/stable/stock/market/batch?symbols=" + ','.join(stock_symbols) + "&types=quote&filter=symbol,latestPrice,change,changePercent&displayPercent=true&token=" + api_token)
json_data = json.loads(response.read())

for stock_symbol in stock_symbols:
    stock_quote = json_data[stock_symbol]["quote"]
    price_current = stock_quote["latestPrice"]
    price_changed = stock_quote["change"]
    price_percent_changed = stock_quote["changePercent"]

    if price_changed is not None:
        color = "red" if float(price_changed) < 0 else "green"
        print("{} {:.2f} {:.2f} ({:.2f}%) | color={}".format(stock_symbol, price_current, price_changed, price_percent_changed, color))
    else:
        color = "black"
        print("{} {:.2f} | color={}".format(stock_symbol, price_current, color))
