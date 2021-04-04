#!/bin/bash

# Shows last Ethereum price in USD.
#
# <xbar.title>Ethereum last price</xbar.title>
# <xbar.version>0.1B</xbar.version>
# <xbar.author>Nikita Zhavoronkov</xbar.author>
# <xbar.author.github>Har01d</xbar.author.github>
# <xbar.desc>Shows last Ethereum price in USD.</xbar.desc>
# <xbar.image>http://i.imgur.com/lF2AA7o.png</xbar.image>
#
# by Nikita Zhavoronkov
# Based on Coinbase bitbar plugin by Mat Ryer

echo -n "𝚵 "; curl -s "https://api.coinbase.com/v2/prices/ETH-USD/spot" | egrep -o '"amount":"[0-9]+(\.)?' | sed 's/"amount"://'  | sed 's:^.\(.*\).$:\1:'