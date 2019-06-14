#!/usr/bin/python
# coding=utf-8

# <bitbar.title>Binance Price Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Gabriel Age</bitbar.author>
# <bitbar.author.github>agezao</bitbar.author.github>
# <bitbar.desc>Displays Binance's ticker price for configured coin pairs</bitbar.desc>
# <bitbar.image>https://i.imgur.com/zJsoTl8.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
from urllib import urlopen

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

    print('{} - {} : {} | templateImage={}'.format(coin_symbol, last_price, price_variation, bitcoin_icon))
