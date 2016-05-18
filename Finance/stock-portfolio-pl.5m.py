#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>Stock Portfolio P/L</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Lukas Steinbrecher</bitbar.author>
# <bitbar.author.github>lukstei</bitbar.author.github>
# <bitbar.desc>Shows the daily profit and loss of your stock portfolio (option to import Interactive Brokers portfolio)</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/lukstei/lukstei.github.io/master/ext/img/stock-portfolio.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://lukstei.com/</bitbar.abouturl>

import urllib2
import json

def ib_portfolio(portfolio_file):
	import csv
	with open(portfolio_file, 'rb') as csvfile:
		reader = csv.DictReader(csvfile)
		d = {}
		for r in reader:
			if r['Security Type'] == 'STK':
				d[r['Contract Description'].upper()] = int(r['Position'])
		return d

def bitbar_main(portfolio):
	q = ','.join([k for k,v in portfolio.iteritems()])
	url = "http://finance.google.com/finance/info?client=ig&q=" + q
	response = urllib2.urlopen(url).read()
	o = json.loads(response[4:-1])

	pl = 0.0
	for ticker in o:
		pl += portfolio[ticker["t"].upper()] * float(ticker["c"])

	print "P/L %+.2f" % pl
	print "---"

	# show individual stocks
	for ticker in o:
	    print "%s %s (%+.2f%%)" % (ticker["t"], ticker["l"], float(ticker["cp"]))


## Option 1: Define your portfolio manually:
## In this example we are long 10 AAPL, short 10 SPY, and long 4 TSLA
portfolio = {'AAPL': 10, 'SPY': -10, 'TSLA': 4}

## Option 2: Import your portfolio from Interactive Brokers TWS
## Go to Account -> Account Window -> File -> Export Portfolio
## Uncomment the next line and add the portfolio file path
# portfolio = ib_portfolio('insert path here')


bitbar_main(portfolio)

