#!/bin/bash

# Shows last BTC price (in USD) on Bitstamp exchange.
#
# <xbar.title>Bitstamp last price</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Damien Lajarretie</xbar.author>
# <xbar.author.github>dlajarretie</xbar.author.github>
# <xbar.desc>Shows last BTC price (in USD) on Bitstamp exchange.</xbar.desc>
# <xbar.image>http://i.imgur.com/aQCqOW6.png</xbar.image>
#
# by Damien Lajarretie
# Based on Coinbase bitbar plugin by Mat Ryer

echo -n "Bitstamp: $"; curl -s "https://www.bitstamp.net/api/ticker/" | egrep -o '"last": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last": //' | sed 's/\"//g'