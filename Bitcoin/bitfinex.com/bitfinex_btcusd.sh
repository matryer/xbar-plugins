#!/bin/bash

# Shows last BTC price (in USD) on Bitfinex BTCUSD Order Book.
#
# <bitbar.title>Bitfinex BTCUSD last price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Roberto Santacroce Martins</bitbar.author>
# <bitbar.author.github>mileschet</bitbar.author.github>
# <bitbar.desc>Shows last BTC price (in USD) on Bitfinex BTCUSD Order Book.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/AJU4wmc.png</bitbar.image>
#
# by Roberto Santacroce Martins
# Based on Coinbase bitbar plugin by Mat Ryer

echo -n "Ƀ$"; curl -s "https://api.bitfinex.com/v1/pubticker/BTCUSD" | egrep -o '"last_price":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last_price"://' | sed 's/\"//g'