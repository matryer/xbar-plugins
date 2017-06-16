#!/bin/bash
#
# <bitbar.title>BTC Markets ETH & BTC Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Fred Wu</bitbar.author>
# <bitbar.author.github>fredwu</bitbar.author.github>
# <bitbar.desc>Displays the latest Etherium and Bitcoin prices in AUD and their 24h trade volumes from BTC Markets.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/7UwilDM.png</bitbar.image>

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
