#!/bin/bash

# <bitbar.title>Bitcoin.de price ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>alex_rockt</bitbar.author>
# <bitbar.author.github>alex_rockt</bitbar.author.github>
# <bitbar.desc>Shows the current bitcoin price for Bitcoins from Bitcoin.de.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/mP8Vec2.png</bitbar.image>
# <bitbar.dependencies>none</bitbar.dependencies>

DATA=$(curl -s "https://bitcoinapi.de/widget/current-btc-price/rate.json")

echo -n "BTC: "; echo "$DATA"  | egrep -o '"price_eur":"[0-9.]+(\,)?([0-9]{0,2}\\)?' | sed 's/"price_eur":"//' | sed 's/\\/ EUR/'
echo "---"
echo "$DATA" | egrep -o '"date_de":"[0-9]{2}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}"' | sed 's/"date_de":"//' | sed 's/\\/ EUR/' | sed 's/"//'
