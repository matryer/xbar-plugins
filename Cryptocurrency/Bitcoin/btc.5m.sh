#!/bin/bash

# <bitbar.title>BTC</bitbar.title>
# <bitbar.image>http://i.imgur.com/V8dABjz.png</bitbar.image>
# <bitbar.author>Tim Paine</bitbar.author>
# <bitbar.author.github>theocean154</bitbar.author.github> 
#

# Bitfinex
RESULT=$(curl -s "https://api.bitfinex.com/v1/pubticker/BTCUSD") 
echo -n "bfx $"; echo "$RESULT" | egrep -o '"last_price":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last_price"://' | sed 's/\"//g' | cut -c 1-6

# Bitstamp
RESULT=$(curl -s "https://www.bitstamp.net/api/v2/ticker/btcusd/") 
echo -n "bst $"; echo "$RESULT" | egrep -o '"last": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last"://' | sed 's/\"//g' | cut -c 1-7

# CEX
RESULT=$(curl -s "https://cex.io/api/ticker/BTC/USD") 
echo -n "cex $"; echo "$RESULT" | egrep -o '"last":"[0-9]+(\.)?([0-9]{0,4}")?' | sed 's/"last"://' | sed 's/\"//g' | cut -c 1-6

# GDAX
RESULT=$(curl -s "https://api.gdax.com/products/btc-usd/ticker") 
echo -n "gdx $"; echo "$RESULT" | egrep -o '"price":"[0-9]+(\.)?([0-9]{0,}")?' | sed 's/"price"://' | sed 's/\"//g' | cut -c 1-6

# Gemini
RESULT=$(curl -s "https://api.gemini.com/v1/pubticker/btcusd") 
echo -n "gem $"; echo "$RESULT" | egrep -o '"last":"[0-9]+(\.)?([0-9]{0,}")?' | sed 's/"last"://' | sed 's/\"//g' | cut -c 1-6

# HitBTC
RESULT=$(curl -s "http://api.hitbtc.com/api/1/public/BTCUSD/ticker") 
echo -n "hit $"; echo "$RESULT" | egrep -o '"last":"[0-9]+(\.)?([0-9]{0,}")?' | sed 's/"last"://' | sed 's/\"//g' | cut -c 1-6

# ItBit
RESULT=$(curl -s "https://api.itbit.com/v1/markets/XBTUSD/ticker")
echo -n "itb $"; echo "$RESULT" | egrep -o '"lastPrice":"[0-9]+(\.)?([0-9]{0,}")?' | sed 's/"lastPrice"://' | sed 's/\"//g' | cut -c 1-6

# Kraken
RESULT=$(curl -s "https://api.kraken.com/0/public/Ticker?pair=XBTUSD")
echo -n "krk $"; echo "$RESULT" | egrep -o '"c":["[0-9]+(\.)?([0-9]{0,}")?' | sed 's/"c":\[//' | sed 's/\"//g' | cut -c 1-6

# LakeBTC
RESULT=$(curl -s "https://api.lakebtc.com/api_v2/ticker")
echo -n "lak $"; echo "$RESULT" | egrep -o '"last":"[0-9]+(\.)?([0-9]{0,})?"' | sed 's/"last"://' | sed 's/\"//g' | head -n 1 | cut -c 1-6 # first is USD
# |  rev | cut -c 3- | rev

# OKCoin
RESULT=$(curl -s "https://www.okcoin.com/api/v1/ticker.do?symbol=btc_usd")
echo -n "okc $"; echo "$RESULT" | egrep -o '"last":"[0-9]+(\.)?([0-9]{0,})?' | sed 's/"last"://' | sed 's/\"//g' | head -n 1 | cut -c 1-6 # first is USD

# Poloniex
RESULT=$(curl -s "https://poloniex.com/public?command=returnTicker")
echo -n "plx $"; echo "$RESULT" | egrep -o '"USDT_BTC":{"id":121,"last":"[0-9]+(\.)?([0-9]{0,})?' | sed 's/"USDT_BTC":{"id":121,"last":"//' | sed 's/\"//g' | cut -c 1-6
