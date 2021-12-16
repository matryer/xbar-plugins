#!/bin/bash

# <xbar.title>Bitcoin.de price ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>alex_rockt</xbar.author>
# <xbar.author.github>alex_rockt</xbar.author.github>
# <xbar.desc>Shows the current bitcoin price for Bitcoins from Bitcoin.de.</xbar.desc>
# <xbar.image>http://i.imgur.com/mP8Vec2.png</xbar.image>
# <xbar.dependencies>none</xbar.dependencies>

DATA=$(curl -s "https://bitcoinapi.de/widget/current-btc-price/rate.json")

echo -n "BTC: "; echo "$DATA"  | egrep -o '"price_eur":"[0-9.]+(\,)?([0-9]{0,2}\\)?' | sed 's/"price_eur":"//' | sed 's/\\/ EUR/'
echo "---"
echo "$DATA" | egrep -o '"date_de":"[0-9]{2}.[0-9]{2}.[0-9]{2} [0-9]{2}:[0-9]{2}"' | sed 's/"date_de":"//' | sed 's/\\/ EUR/' | sed 's/"//'
