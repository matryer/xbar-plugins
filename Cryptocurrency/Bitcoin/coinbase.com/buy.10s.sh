#!/bin/bash
# <xbar.title>Coinbase.com Buy</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Shows latest buy values (in USD) for Bitcoins in the Coinbase exchange.</xbar.desc>

echo -n "Buy: $"; curl -s "https://coinbase.com/api/v1/prices/buy?currency=USD" | egrep -o ',"amount":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/,"amount"://'  | sed 's:^.\(.*\).$:\1:'
