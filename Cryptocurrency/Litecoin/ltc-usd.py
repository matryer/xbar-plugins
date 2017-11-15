#!/usr/bin/env python
# coding=utf-8
#
# <bitbar.title>Litecoin USD Tracker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>mgjo5899</bitbar.author>
# <bitbar.author.github>mgjo5899</bitbar.author.github>
# <bitbar.desc>It tracks Litecoin price in USD</bitbar.desc>
# <bitbar.image>https://i.imgur.com/OI9eD75.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by impshum

import requests
import base64

indicator = '<span class="text-large" id="quote_price">'
r = requests.get('https://coinmarketcap.com/currencies/litecoin/')
s_i = r.text.find(indicator)
temp = r.text[s_i+len(indicator):]
price = temp[:temp.find('<')]

print(price + " | image=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")
