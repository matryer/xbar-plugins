#!/bin/bash

# Shows last BTC price (in USD) on Bitfinex BTCUSD Order Book.
#
# <xbar.title>Bitfinex BTCUSD last price</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Roberto Santacroce Martins</xbar.author>
# <xbar.author.github>mileschet</xbar.author.github>
# <xbar.desc>Shows last BTC price (in USD) on Bitfinex BTCUSD Order Book.</xbar.desc>
# <xbar.image>http://i.imgur.com/AJU4wmc.png</xbar.image>
#
# by Roberto Santacroce Martins
# Based on Coinbase bitbar plugin by Mat Ryer

echo -n "Ƀ$"; curl -s "https://api.bitfinex.com/v1/pubticker/BTCUSD" | egrep -o '"last_price":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last_price"://' | sed 's/\"//g'