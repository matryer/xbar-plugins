#!/usr/bin/env python
#

# <bitbar.title>Realtime Stock Tracker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Bogdan Mosincat</bitbar.author>
# <bitbar.author.github>bogdan1304</bitbar.author.github>
# <bitbar.desc>Shows realtime stock price and daily percentage change for each stock in the list.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/hQoCXFL.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json, urllib2

def get_stock_price(stock):
	response = urllib2.urlopen('https://api.iextrading.com/1.0/stock/' + stock + '/quote')
	return json.loads(response.read())

def create_output_string(stock):
	output = stock
	output += " - $"
	output += "{:0.2f}".format(response["latestPrice"])
	output += " (" + "{:0.2f}".format(response["changePercent"] * 100.00) + "%)"

	color = "red" if response["changePercent"] < 0 else "green"
	output += " | color=" + color

	return output

menubar_stock = "SPY"
response = get_stock_price(menubar_stock)
print create_output_string(menubar_stock)
print '---'

stocks = ["SPY", "AAPL", "FB", "QTM", "GOOGL", "AMZN", "NFLX", "JCP"]
for stock in stocks:
	response = get_stock_price(stock)
	print create_output_string(stock)
