#!/bin/bash

# Shows last Ripple (XRP) price in USD.
#
# <bitbar.title>XRP last price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Kevin Bost</bitbar.author>
# <bitbar.author.github>kevbost</bitbar.author.github>
# <bitbar.desc>Shows last Ripple (XRP) price in USD.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/iAMBYVv.png</bitbar.image>
#
# by Kevin Bost
# Based on Ethereum bitbar plugin by Nikita Zhavoronkov

echo -n "ˣʳᵖ"; curl -s "https://coinmarketcap-nexuist.rhcloud.com/api/xrp/price" | egrep -o '"usd":[0-9]+(\.)?([0-9]{2})?' | sed 's/"usd"://' | sed 's/\"//g'
