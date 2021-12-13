#!/usr/bin/env python
# -*- coding: utf-8 -*-
# <xbar.title>$GUILD Price Tracker</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Rahmat Ramadhan [rahmat@blockchainspace.asia]</xbar.author>
# <xbar.author.github>rririanto</xbar.author.github>
# <xbar.desc>$GUILD price tracker</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>https://i.imgur.com/OUTglzf.png</xbar.image>
# <xbar.abouturl>https://github.com/rririanto</xbar.abouturl>

import urllib2
import json


guild_symbol = "blockchainspace"
crypto_symbols = {"usd": "$", "eur": "€", "php": "₱", "hkd": "HK$", "sgd": "SGD$", "idr": "Rp"}
#-----------------------------------------------------------------------------

message = []
for currency, val in crypto_symbols.items():
    response = urllib2.urlopen("https://api.coingecko.com/api/v3/simple/price?ids=%s&vs_currencies=%s&include_24hr_change=true" % (guild_symbol, currency))
    result = json.loads(response.read())

    guild_price = result[guild_symbol]
    price_current = guild_price.get(currency, 'N/A')
    price_changed = guild_price.get('%s_24h_change' % currency, 'N/A')

    if price_changed is not None:
        color = "red" if float(price_changed) < 0 else "green"
        message.append("$GUILD-{} {}{:.2f} ({:.2f}%) | color={} size=13".format(currency.upper(), val, price_current, price_changed, color))

print (message[0])
print ("---")
print (message[4])
print (message[3])
print (message[2])
print (message[1])
print ("---")
print ("More @Coingecko | href=https://www.coingecko.com/en/coins/blockchainspace")
print ("Visit BlockchainSpace site | href=https://blockchainspace.asia")
