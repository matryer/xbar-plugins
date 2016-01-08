#!/bin/bash

# Winkdex.com Buy
# BitBar Plugin
#
# by Baron Reznik
#
# Shows latest buy values (in USD) for Bitcoins 
# based on the latest price according to WinkDex

PRICE=$(curl -s -H "Accept-Encoding: gzip" "https://winkdex.com/api/v0/price" | gzcat | python -c 'import json,sys;obj=json.load(sys.stdin); print obj["price"]';)
echo -n "WinkDex: $"; echo "$((PRICE/100)).$((PRICE%100))";
echo "---";
echo "Data Provided by WINKDEX(SM) | href=https://winkdex.com/";
