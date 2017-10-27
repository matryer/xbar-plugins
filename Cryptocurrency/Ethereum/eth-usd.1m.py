#!/usr/bin/python
# coding=utf-8
#
# <bitbar.title>Ethereum USD Price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Charles Lehnert</bitbar.author>
# <bitbar.author.github>CLL80</bitbar.author.github>
# <bitbar.desc>Displays current Ethereum price in USD rounded to the whole dollar from api.coinmarketcap.</bitbar.desc>
# <bitbar.image>https://raw.github.com/CLL80/bitbar-assets/master/screenshots/Cryptocurrency/Ethereum/eth-usd.1m.png</bitbar.image>

from urllib import urlopen
url = urlopen('https://api.coinmarketcap.com/v1/ticker/ethereum/').read()

import json
result = json.loads(url)[0]
symbol = '𝚵 '
price = str(int(round(float(result['price_usd']))))
print(symbol + price)
