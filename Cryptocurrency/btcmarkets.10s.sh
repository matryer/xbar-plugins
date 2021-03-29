#!/bin/bash
#
# <xbar.title>BTC Markets ETH & BTC Ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Fred Wu</xbar.author>
# <xbar.author.github>fredwu</xbar.author.github>
# <xbar.desc>Displays the latest Etherium and Bitcoin prices in AUD and their 24h trade volumes from BTC Markets.</xbar.desc>
# <xbar.image>http://i.imgur.com/7UwilDM.png</xbar.image>

echo -n "ETH "; curl -s https://api.btcmarkets.net/market/ETH/AUD/tick | \
  grep -Eo 'lastPrice":[0-9\.]+' | sed 's/lastPrice"://' | tr -d '\n'; \
  echo -n " BTC "; curl -s https://api.btcmarkets.net/market/BTC/AUD/tick | \
  grep -Eo 'lastPrice":[0-9\.]+' | sed 's/lastPrice"://'

echo '---'
echo 'Volume 24h'

for COIN_TYPE in 'ETH' 'BTC'
do
  echo -n "$COIN_TYPE "; \
    curl -s "https://api.btcmarkets.net/market/$COIN_TYPE/AUD/tick" | \
    grep -Eo 'volume24h":[0-9\.]+' | sed 's/volume24h"://' | tr -d '\n'; \
    echo '| href=https://www.btcmarkets.net/'
done
