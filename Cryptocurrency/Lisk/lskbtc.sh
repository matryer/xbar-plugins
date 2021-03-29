#!/bin/bash

# Shows last Lisk price on Poloniex in BTC.
#
# <xbar.title>Lisk/BTC Poloniex price</xbar.title>
# <xbar.version>0.1</xbar.version>
# <xbar.author>Corvin Wimmer</xbar.author>
# <xbar.author.github>corv89</xbar.author.github>
# <xbar.desc>Shows last Lisk price on Bitfinex in BTC.</xbar.desc>
# <xbar.image>http://i.imgur.com/j5qaX2t.png</xbar.image>

echo -n "LSK "; curl -s https://poloniex.com/public\?command=returnTicker | grep -Eo '"BTC_LSK":{"id":163,"last":"[0-9]+\.[0-9]+' | sed 's/"BTC_LSK":{"id":163,"last":"//'
