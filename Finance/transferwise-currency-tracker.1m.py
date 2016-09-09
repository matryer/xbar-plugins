#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <bitbar.title>Currency Tracker Transferwise</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Andrew Keating</bitbar.author>
# <bitbar.author.github>andrewzk</bitbar.author.github>
# <bitbar.desc>Keep an eye on Transferwise currency exchange rates</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>http://andrewzk.github.io/gh-pages/transferwise.png</bitbar.image>

import urllib2
import json

TRANSFERWISE_KEY = "dad99d7d8e52c2c8aaf9fda788d8acdc"

# Replace with desired currencies
currency_from = 'USD'
currency_to = 'DKK'

url = "https://transferwise.com/api/v1/payment/calculate?amount=1" \
      "&amountCurrency=source&hasDiscount=false&isFixedRate=false" \
      "&isGuaranteedFixedTarget=false" \
      "&sourceCurrency={}&targetCurrency={}".format(currency_from, currency_to)

req = urllib2.Request(url)
req.add_header('X-Authorization-key', TRANSFERWISE_KEY)

result = json.loads(urllib2.urlopen(req).read())['transferwiseRate']

print "{}: {:.2f}".format(currency_to, result)
print "---"
print "From: {}".format(currency_from)
