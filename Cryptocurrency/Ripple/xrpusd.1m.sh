#!/bin/bash

# Shows last Ripple (XRP) price in USD.
#
# <xbar.title>XRP last price</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kevin Bost</xbar.author>
# <xbar.author.github>kevbost</xbar.author.github>
# <xbar.desc>Shows last Ripple (XRP) price in USD.</xbar.desc>
# <xbar.image>http://i.imgur.com/iAMBYVv.png</xbar.image>
#
# by Kevin Bost
# Based on Ethereum bitbar plugin by Nikita Zhavoronkov

echo -n "ˣʳᵖ"; curl -s "https://coinmarketcap-nexuist.rhcloud.com/api/xrp/price" | egrep -o '"usd":[0-9]+(\.)?([0-9]{2})?' | sed 's/"usd"://' | sed 's/\"//g'
