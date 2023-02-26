#!/usr/bin/python3
# coding=utf-8

# <xbar.title>Solana Price Ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Gabriel Serafini</xbar.author>
# <xbar.author.github>gserafini</xbar.author.github>
# <xbar.desc>Displays crypto ticker price for Solana (can be adapted to other cryptocurrency as needed).</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/200852/221348165-f296f635-7b80-401c-9d96-cb1f51cbe88e.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/gserafini/xbar-plugin-crypto-price-tracker</xbar.abouturl>

# New API URL: https://www.coingecko.com/en/api/documentation

import json
import urllib.request

# List here the symbols you want to keep track:
coin_symbols = ['solana']

# To get a list of available symbols check all the "symbol" attributes here:
# https://api.binance.com/api/v1/ticker/24hr
#
# new url = https://api.coingecko.com/api/v3/simple/price?ids=solana&vs_currencies=usd

for coin_symbol in coin_symbols:

	url = "https://api.coingecko.com/api/v3/simple/price?vs_currencies=usd&include_24hr_change=true&ids={}".format(coin_symbol)
    
	try:
	    payload = urllib.request.urlopen(url)
	    data = json.load(payload)
	    last_price = data['solana']['usd']
	    price_variation = round(data['solana']['usd_24h_change'], 2)
	    print('{} ≈ ${} ({}%)'.format("◎", last_price, price_variation))

	except Exception as e:
		print("Offline")
# 	    print(e.reason)

