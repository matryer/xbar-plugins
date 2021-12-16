#!/bin/bash

# Shows last Ethereum price on Bitfinex in BTC.
#
# <xbar.title>Ethereum BFX price</xbar.title>
# <xbar.version>0.1</xbar.version>
# <xbar.author>Corvin Wimmer</xbar.author>
# <xbar.author.github>corv89</xbar.author.github>
# <xbar.desc>Shows last Ethereum price on Bitfinex in BTC.</xbar.desc>
# <xbar.image>http://i.imgur.com/w6D7HHe.png</xbar.image>

echo -n "ETH "; curl -s "https://api.bitfinex.com/v1/pubticker/ETHBTC" | grep -Eo 'last_price":"[0-9]+\.[0-9]+' | sed 's/last_price":"//'