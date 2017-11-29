#!/bin/bash

# <bitbar.title>Bitso buys price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Erick Madrid</bitbar.author>
# <bitbar.author.github>Neovirxp</bitbar.author.github>
# <bitbar.desc>Displays Bitso buy prices of Bitcoin, Ethereum, Ripple in MXN</bitbar.desc>
# <bitbar.image>https://image.ibb.co/h9mWFG/Screen_Shot_2017_11_28_at_7_59_08_PM.png</bitbar.image>
# <bitbar.dependencies>bash, jq</bitbar.dependencies>

# If you feel this little tool gives you some value, tips are always welcome at the following addresses!
# Bitcoin: 19sFQuSb1cwGA9Cb5XYVMHeYUVW8ME6ki8
# Ethereum: 0xF8D3C6dd50536eC8FF61CCE5C47F8191f55c9B81

export LANG='en_US.UTF-8'
export LC_NUMERIC='en_US'

export PATH="/usr/local/bin/:$PATH"

BTC_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=btc_mxn')
ETH_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=eth_mxn')
XRP_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=xrp_mxn')

BTC_NOW=$(echo "$BTC_MXN" | jq '.payload.last' | tr -d \")
ETH_NOW=$(echo "$ETH_MXN" | jq '.payload.last' | tr -d \")
XRP_NOW=$(echo "$XRP_MXN" | jq '.payload.last' | tr -d \")

printf "BTC: %'.0f ETH: %'.0f XRP: %'.2f | color=green \n" "$BTC_NOW" "$ETH_NOW" "$XRP_NOW"

echo '---'

echo "$BTC_NOW" | xargs printf "1 BTC = %'.2f MXN  | color=green \n"

echo '---'

# last 24 hours price high
echo "$BTC_MXN" | jq '.payload.high' | tr -d \" | xargs printf "HIGH\t$%'.2f MXN\n"
# last 24 hours price low
echo "$BTC_MXN" | jq '.payload.low' | tr -d \"  | xargs printf "LOW \t$%'.2f MXN\n"

echo '---'

# lowest sell order
echo "$BTC_MXN" | jq '.payload.ask' | tr -d \" | xargs printf "ASK\t\t$%'.2f MXN\n"
# highest buy order
echo "$BTC_MXN" | jq '.payload.bid' | tr -d \" | xargs printf "BID\t\t$%'.2f MXN\n"

echo '---'

echo "$ETH_NOW" | xargs printf "1 ETH = %'.2f MXN | color=blue \n"

echo '---'

# last 24 hours price high
echo "$ETH_MXN" | jq '.payload.high' | tr -d \" | xargs printf "HIGH\t$%'.2f MXN\n"
# last 24 hours price low
echo "$ETH_MXN" | jq '.payload.low' | tr -d \"  | xargs printf "LOW \t$%'.2f MXN\n"

echo '---'

# lowest sell order
echo "$ETH_MXN" | jq '.payload.ask' | tr -d \" | xargs printf "ASK\t\t$%'.2f MXN\n"
# highest buy order
echo "$ETH_MXN" | jq '.payload.bid' | tr -d \" | xargs printf "BID\t\t$%'.2f MXN\n"

echo '---'

echo "$XRP_NOW" | xargs printf "1 XRP = %'.2f MXN | color=red \n"

echo '---'

# last 24 hours price high
echo "$XRP_MXN" | jq '.payload.high' | tr -d \" | xargs printf "HIGH\t$%'.2f MXN\n"
# last 24 hours price low
echo "$XRP_MXN" | jq '.payload.low' | tr -d \"  | xargs printf "LOW \t$%'.2f MXN\n"

echo '---'

# lowest sell order
echo "$XRP_MXN" | jq '.payload.ask' | tr -d \" | xargs printf "ASK\t\t$%'.2f MXN\n"
# highest buy order
echo "$XRP_MXN" | jq '.payload.bid' | tr -d \" | xargs printf "BID\t\t$%'.2f MXN\n"

