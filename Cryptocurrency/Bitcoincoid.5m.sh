#!/bin/bash

# <bitbar.title>Bitcoin.co.id Spot Market Prices</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Hari Prasetyo</bitbar.author>
# <bitbar.author.github>hapr05</bitbar.author.github>
# <bitbar.desc>Display the spot IDR prices of cryptocurrencies from bitcoin.co.id</bitbar.desc>
# <bitbar.image>https://i.imgur.com/4uhAq0p.png</bitbar.image>

# Based on the Cryptocurrency Prices plugin by viiraj

# If you ❤ this plugin and you're feeling generous ☺, you can tip me here:
# Bitcoin: 1Lt9eNbiiLGAc4kxr2NQm2MREij8YW54kU
# Ethereum: 0x70641992c96f7bf1ad4e9ffc9a29b8f2917b8249

# define coins
coins=( "btc_idr:BTC" "bch_idr:BCH" "btg_idr:BTG" "eth_idr:ETH" "bch_idr:BCH" "etc_idr:ETC" "ignis_idr:IGNIS" "ltc_idr:LTC" "nxt_idr:NXT" "waves_idr:WAVES" "str_idr:XLM" "xrp_idr:XRP" "xzc_idr:XZC" )

# set locale for thousand separator
LC_NUMERIC=en_US

# print them values
for coin in "${coins[@]}"; do
    printf "%s: %'.f\n" "${coin#*:}/IDR" "$(curl -s https://vip.bitcoin.co.id/api/${coin%%:*}/ticker | tr -d '"' | tr ':,' '\n' | grep -A1 "last" | tail -1)"; 
done
