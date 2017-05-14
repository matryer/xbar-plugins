#!/bin/bash
# <bitbar.title>Coinbase.com Your Balance</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Shows your balance in BTC.  Be sure you add you API key.</bitbar.desc>

echo -n "BTC: "; curl -s "https://coinbase.com/api/v1/account/balance?api_key=YOUR_API_KEY" | egrep -o '"amount":"[0-9]+(\.)?([0-9]+").*?' | sed 's/"amount"://' | sed 's:^.\(.*\).$:\1:'
