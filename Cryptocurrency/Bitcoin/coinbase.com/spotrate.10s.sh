#!/bin/bash
# <xbar.title>Coinbase.com Spot rate</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Mat Ryer</xbar.author>
# <xbar.author.github>matryer</xbar.author.github>
# <xbar.desc>Shows latest spot rate values (in USD) for Bitcoins in the Coinbase exchange.</xbar.desc>

echo -n "BTC: $"; curl -s "https://coinbase.com/api/v1/prices/spot_rate?currency=USD" | egrep -o '"amount":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"amount"://'  | sed 's:^.\(.*\).$:\1:'
