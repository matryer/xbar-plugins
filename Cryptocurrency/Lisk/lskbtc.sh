#!/bin/bash

# Shows last Lisk price on Poloniex in BTC.
#
# <bitbar.title>Lisk/BTC Poloniex price</bitbar.title>
# <bitbar.version>0.1</bitbar.version>
# <bitbar.author>Corvin Wimmer</bitbar.author>
# <bitbar.author.github>corv89</bitbar.author.github>
# <bitbar.desc>Shows last Lisk price on Bitfinex in BTC.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/j5qaX2t.png</bitbar.image>

echo -n "LSK "; curl -s https://poloniex.com/public\?command=returnTicker | grep -Eo '"BTC_LSK":{"id":163,"last":"[0-9]+\.[0-9]+' | sed 's/"BTC_LSK":{"id":163,"last":"//'
