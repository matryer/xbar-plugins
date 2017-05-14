#!/bin/bash
# <bitbar.title>Winkdex.com Buy</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Baron Reznik</bitbar.author>
# <bitbar.author.github>xiezusan</bitbar.author.github>
# <bitbar.desc>Shows latest buy values (in USD) for Bitcoins based on the latest price according to WinkDex.</bitbar.desc>

PRICE=$(curl -s -H "Accept-Encoding: gzip" "https://winkdex.com/api/v0/price" | gzcat | python -c 'import json,sys;obj=json.load(sys.stdin); print obj["price"]';)
echo -n "WinkDex: $"; echo "$((PRICE/100)).$((PRICE%100))";
echo "---";
echo "Data Provided by WINKDEX(SM) | href=https://winkdex.com/";
