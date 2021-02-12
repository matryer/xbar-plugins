#!/bin/bash

# <bitbar.title>Bitcoin Price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Javier Aceña</bitbar.author>
# <bitbar.author.github>j0nl1</bitbar.author.github>
# <bitbar.desc>This plugin displays the current price of bitcoin in USD from Binance platform</bitbar.desc>
# <bitbar.image>https://i.imgur.com/npWiJRn.png/bitbar.image>
#

RESULT=$(curl -s "https://api.binance.com/api/v1/ticker/price?symbol=BTCUSDT")

echo "$RESULT" | egrep -o '[0-9]+(\.)' | sed '$ s/\./$/'