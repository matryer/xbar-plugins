#!/bin/bash

# Shows last Ethereum price on Bitfinex in BTC.
#
# <bitbar.title>Ethereum BFX price</bitbar.title>
# <bitbar.version>0.1</bitbar.version>
# <bitbar.author>Corvin Wimmer</bitbar.author>
# <bitbar.author.github>corv89</bitbar.author.github>
# <bitbar.desc>Shows last Ethereum price on Bitfinex in BTC.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/w6D7HHe.png</bitbar.image>

echo -n "ETH "; curl -s "https://api.bitfinex.com/v1/pubticker/ETHBTC" | grep -Eo 'last_price":"[0-9]+\.[0-9]+' | sed 's/last_price":"//'