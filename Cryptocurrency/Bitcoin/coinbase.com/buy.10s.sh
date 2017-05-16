#!/bin/bash
# <bitbar.title>Coinbase.com Buy</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Shows latest buy values (in USD) for Bitcoins in the Coinbase exchange.</bitbar.desc>

echo -n "Buy: $"; curl -s "https://coinbase.com/api/v1/prices/buy?currency=USD" | egrep -o ',"amount":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/,"amount"://'  | sed 's:^.\(.*\).$:\1:'
