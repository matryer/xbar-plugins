#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <xbar.title>Currency Tracker Transferwise</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Andrew Keating</xbar.author>
# <xbar.author.github>andrewzk</xbar.author.github>
# <xbar.desc>Keep an eye on Transferwise currency exchange rates</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>http://andrewzk.github.io/gh-pages/transferwise.png</xbar.image>

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
