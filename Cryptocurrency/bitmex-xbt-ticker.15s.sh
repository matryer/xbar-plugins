#!/bin/sh

# <bitbar.title>BitMEX XBT Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>horimislime</bitbar.author>
# <bitbar.author.github>horimislime</bitbar.author.github>
# <bitbar.desc>Displays latest XBTUSD ticker on BitMEX</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/horimislime/bitbar-plugin-bitmex-ticker/master/assets/screenshot.png</bitbar.image>
# <bitbar.dependencies>bash<bitbar.dependencies>

API_URL='https://www.bitmex.com/api/v1/quote?symbol=XBT&count=1&reverse=true'
JSON=$(curl -s $API_URL)

get_value() {
    echo $1 | 
    sed 's/^\[{\(.*\)}]$/\1/' | tr ',' '\n' |
    egrep $2 |
    sed 's/^.*://' | tr -d '"'
}

ASK_SIZE=$(get_value $JSON "askSize")
BID_SIZE=$(get_value $JSON "bidSize")
ASK_PRICE=$(get_value $JSON "askPrice")
BID_PRICE=$(get_value $JSON "bidPrice")
MID_PRICE=$(echo "scale = 1; ($ASK_PRICE + $BID_PRICE) / 2" | bc)

echo "\xE2\x82\xBF $MID_PRICE"
echo "---"

echo "Ask:\t$ASK_PRICE\tSize: $ASK_SIZE |color=green"
echo "Bid:\t$BID_PRICE\tSize: $BID_SIZE |color=red"
echo "---"

echo "Open bitmex.com | href=\"https://www.bitmex.com\""
