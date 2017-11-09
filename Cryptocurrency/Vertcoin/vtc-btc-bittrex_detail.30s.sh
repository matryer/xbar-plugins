#!/bin/bash

# <bitbar.title>Vertcoin BTC price at Bittrex</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cem Yildirim</bitbar.author>
# <bitbar.author.github>cemyld</bitbar.author.github>
# <bitbar.desc>Shows the last Vertcoin price (in BTC) on the Bittrex exchange.</bitbar.desc>
# <bitbar.image>https://imgur.com/DEOGHG6.jpg</bitbar.image>
# <bitbar.abouturl>https://www.vertcoin.org/bitbar.abouturl>
#


vtcIconBase64="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"
RESULT=$(curl -s "https://bittrex.com/api/v1.1/public/getmarketsummary?market=btc-vtc")
vtcLast=$(echo $RESULT | egrep -o '"Last":\d+\.?\d{0,5}' | sed 's/"Last"://')
printf "%.*f | image=%s\n" 5 "$vtcLast" "$vtcIconBase64"
echo "---"
echo -n "Ask "; echo -n "$RESULT" | egrep -o '"Ask":\d+\.?\d{0,6}' | sed 's/"Ask"://' | sed 's/$/Ƀ/';
echo -n "Bid "; echo "$RESULT" | egrep -o '"Bid":\d+\.?\d{0,6}' | sed 's/"Bid"://' | sed 's/$/Ƀ/';
echo -n "Low "; echo "$RESULT" | egrep -o '"Low":\d+\.?\d{0,6}' | sed 's/"Low"://' | sed 's/$/Ƀ/';
echo -n "High "; echo "$RESULT" | egrep -o '"High":\d+\.?\d{0,6}' | sed 's/"High"://' | sed 's/$/Ƀ/';
echo -n "Volume "; echo "$RESULT" | egrep -o '"Volume":\d+\.?\d{0,2}' | sed 's/"Volume"://' | sed 's/$/Ƀ/';
echo -n "Updated At "; date  +"%F %T"
echo "Live Graph | href=\"https://bittrex.com/Market/Index?MarketName=BTC-VTC\""
