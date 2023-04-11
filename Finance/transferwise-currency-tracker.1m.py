#!/usr/bin/env python3

# <xbar.title>Currency Tracker Transferwise</xbar.title>
# <xbar.version>1.1</xbar.version>
# <xbar.author>Andrew Keating</xbar.author>
# <xbar.author.github>andrewzk</xbar.author.github>
# <xbar.desc>Keep an eye on Transferwise currency exchange rates</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>http://andrewzk.github.io/gh-pages/transferwise.png</xbar.image>
# <xbar.var>string(VAR_CURRENCY_FROM="USD"): The currency you are converting from.</xbar.var>
# <xbar.var>string(VAR_CURRENCY_TO="DKK"): The currency you are converting to.</xbar.var>
# <xbar.var>string(VAR_CURRENCY_FROM_LABEL="🇺🇸USD"): The currency you are converting from.</xbar.var>
# <xbar.var>string(VAR_CURRENCY_TO_LABEL="🇩🇰DKK"): The currency you are converting to.</xbar.var>

import urllib.request, urllib.error, urllib.parse
import json
import os

currency_from = os.environ.get("VAR_CURRENCY_FROM")
currency_to = os.environ.get("VAR_CURRENCY_TO")
currency_from_label = os.environ.get("VAR_CURRENCY_FROM_LABEL")
currency_to_label = os.environ.get("VAR_CURRENCY_TO_LABEL")

TRANSFERWISE_KEY = "dad99d7d8e52c2c8aaf9fda788d8acdc"

url = "https://wise.com/api/v1/payment/calculate?amount=1" \
      "&amountCurrency=source&hasDiscount=false&isFixedRate=false" \
      "&isGuaranteedFixedTarget=false" \
      "&sourceCurrency={}&targetCurrency={}".format(currency_from, currency_to)

req = urllib.request.Request(url)
req.add_header('X-Authorization-key', TRANSFERWISE_KEY)

result = json.loads(urllib.request.urlopen(req).read())['transferwiseRate']

print("{}: {:.2f}".format(currency_to_label, result))
print("---")
print("From: {}".format(currency_from_label))
