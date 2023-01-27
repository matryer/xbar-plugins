#!/usr/bin/env python3

# <xbar.title>Binance Price Ticker</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Gabriel Age</xbar.author>
# <xbar.author.github>agezao</xbar.author.github>
# <xbar.desc>Displays Binance's ticker price for configured coin pairs</xbar.desc>
# <xbar.image>https://i.imgur.com/zJsoTl8.jpg</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import json
from urllib.request import urlopen

bitcoin_icon='iVBORw0KGgoAAAANSUhEUgAAACQAAAAkCAQAAABLCVATAAAACXBIWXMAABYlAAAWJQFJUiTwAAABY0lEQVRIx2P4z0AdyEBzg1DAdIYfQJgCZHmCWdsYMAFRBs0BC2UAWT5g1p6hbZAggwIcrgALVQNZSWDWAQY24g3qwRtJ/xgeMqxkCGJgotQgGLzAoEUdg/4zvGQQIxzYLAyODF/gQv0MlgwWDK4MOQxbgV5DKG0nLtZ2wIUykII2EMmoU8QZtAWrQQwMB+HiDygzaDNc/CQlBskwfIKLN5JrkAxDFsMTuOh9BiFSDXoHDI2HDB9RlJ1kECc2r20hkI5OMXhQxyAQzCTNoDJgaAgAvaLLEMkwn+EbkuLvDBLkR78yUoD/Z0gn3yAGhnwk5V2UGBRGLYNmICkvIGzQLqwG8TA0oJQAVvgMymcoYehg+AUXWgoM0kygWC/DbpQ4+89wjYERt0FiRNeNX4GlFJ505EykMacZDPGn7HwCBnxiOMcwjcGJcOEvzqADh2vBQk1AVhaYdZCBc7TKpqJBA9ZiAwDMH49EXcmY2QAAAABJRU5ErkJggg=='

#List here the symbols you want to keep track:
coin_symbols=['BNBBTC', 'ETHBTC']
#To get a list of available symbols check all the "symbol" attributes here:
#https://api.binance.com/api/v1/ticker/24hr

for coin_symbol in coin_symbols:
    url="https://api.binance.com/api/v1/ticker/24hr?symbol={}".format(coin_symbol)
    payload=urlopen(url)

    data = json.load(payload)

    last_price=data['lastPrice']
    price_variation=str(data['priceChangePercent']) + '%'

    print(('{} - {} : {} | templateImage={}'.format(coin_symbol, last_price, price_variation, bitcoin_icon)))
