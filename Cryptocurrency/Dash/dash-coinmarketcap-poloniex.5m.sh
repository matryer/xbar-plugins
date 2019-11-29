#!/bin/bash

# <bitbar.title>Dash tickers: Coinmarketcap and Poloniex</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>UdjinM6</bitbar.author>
# <bitbar.author.github>UdjinM6</bitbar.author.github>
# <bitbar.desc>Shows the latest Dash info from Coinmarketcap and Poloniex</bitbar.desc>
# <bitbar.abouturl>https://www.dash.org/</bitbar.abouturl>

# To generate the image, grab PNG and increase the DPI from 72 to 144,
# then resize it to 32x32 and finally encode the image into base 64
# for example via https://base64.guru/converter/encode/image/png
iconBase64='iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4wsdAAwGzHGX5gAAAg5JREFUWMPt1z1oVEEQB/DfhRON+JXCmGKDIoiKmEIhIIhKCmsLwSKihb0EXikWChYi1ykIQggIFjaCglUghWJhLCwUFItYPPwKQvBiJDEhFnkHIu/d7TsuscnAcTA7uzPz35n/7GNd/rNUoqxq6QhGSpw7i3f4hQk8lYQveYbVyAMPYHfJ5A5l/+ezJF5mSUxKwmLDqCsi+y3Y2wG0B/ECd9XS7jIIbMbODl77JWxqINMVsWE79uB3i98CliKDGFZLT8Yi8B1J5qCZLKMbIauXY9if6fOK/RYGK6vSW7W0msF8EbebJLavsuqNXkuv42rOSh1DXWvANfcxV0ABfWsRwCymCmpmvvoXVGcwip4Sh8/jsCR8aMG2eVe9hB/VzPkRPGoju0lMR7Rxf45+AVONK7jcJrxvJGGmhU0PtuboFyXhWyOAC204r+NhhN2JAv3blUqspf34mrFZzPSs4xNGJWEiYs/ZAv3jRit8xkBWlTGyhAVJ+BnBAf04WrA6uhLAymicXqUWHCvQP5eEeuwwapcBb2CoYPVK2QdJGccHcQenCizG8ap5ALV0Q3ZAb4TLZWzEcZzOpmEzVrwpCXOtEOjDvTaeYa3kmiSMx7wJd2Bbh50PS8KDf5VFRdhbciY0kxTn8pw3Q2CgA46f4QnGJKGwzYsC2IX3JdhxJqvs1/iY7a1Lwuz6p9e6tJI/DAh9aD+iV7oAAAAASUVORK5CYII='

infoPoloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DASH | tr -d '{}"' | tr ':,' '\n')
infoCoinmarketcap=$(curl -s https://api.coinmarketcap.com/v1/ticker/dash/ | tr -d '{}[]", ' | tr ':' '\n')
infoCoinmarketcapBTC=$(curl -s https://api.coinmarketcap.com/v1/ticker/bitcoin/ | tr -d '{}[]", ' | tr ':' '\n')

token_price_btc=$(echo "$infoPoloniex" | grep -A1 last | tail -1)
token_price_btc_precision=6
printf "%.*f | dropdown=false image=%s\n" $token_price_btc_precision $token_price_btc "$iconBase64"
token_price_usd=$(echo "$infoCoinmarketcap" | grep -A1 price_usd | tail -1)
[ $(echo $token_price_usd '>=' 100 | bc -l) -gt 0 ] && token_price_usd_precision=$(($token_price_btc_precision-3)) || token_price_usd_precision=$(($token_price_btc_precision-2))
printf "$%.*f | dropdown=false image=%s\n" $token_price_usd_precision $token_price_usd "$iconBase64"

echo "---"
percent_change_24hBTC=$(echo "$infoCoinmarketcapBTC" | grep -A1 percent_change_24h | tail -1)
[ $(echo $percent_change_24hBTC '>=' 0 | bc -l) -gt 0 ] && colorCoinmarketcapBTC="green" || colorCoinmarketcapBTC="red"
printf ":moneybag: BTC: $%.*f | color=$colorCoinmarketcapBTC href=\"http://coinmarketcap.com/currencies/bitcoin/\"\n" 2 $(echo "$infoCoinmarketcapBTC" | grep -A1 price_usd | tail -1)
echo "---"
echo ":chart_with_upwards_trend: Coinmarketcap: | href=\"http://coinmarketcap.com/currencies/dash/\""
percent_change_24h=$(echo "$infoCoinmarketcap" | grep -A1 percent_change_24h | tail -1)
[ $(echo $percent_change_24h '>=' 0 | bc -l) -gt 0 ] && colorCoinmarketcap="green" || colorCoinmarketcap="red"
printf "%sRank: %.*f\n" "* " 0 $(echo "$infoCoinmarketcap" | grep -A1 rank | tail -1)
available_supply=$(echo "$infoCoinmarketcap" | grep -A1 available_supply | tail -1)
available_supply_mln=$(echo "$available_supply / 1000000" | bc -l)
printf "%sSupply: %.*fM DASH\n" "* " 2 $available_supply_mln
market_cap_usd=$(echo "$infoCoinmarketcap" | grep -A1 market_cap_usd | tail -1)
market_cap_usd_mln=$(echo "$market_cap_usd / 1000000" | bc -l)
printf "%sMarket Cap: $%.*fM | color=$colorCoinmarketcap\n" "* " 2 $market_cap_usd_mln
printf "%sPrice: $%.*f | color=$colorCoinmarketcap\n" "* " 2 $(echo "$infoCoinmarketcap" | grep -A1 price_usd | tail -1)
printf "%s24h Change: %.*f%% | color=$colorCoinmarketcap\n" "* " 2 $percent_change_24h
volume_usd=$(echo "$infoCoinmarketcap" | grep -A1 24h_volume_usd | tail -1)
volume_usd_m=$(echo "$volume_usd / 1000000" | bc -l)
printf "%s24h Volume: $%.*fM\n" "* " 2 $volume_usd_m
last_updated=$(echo "$infoCoinmarketcap" | grep -A1 last_updated | tail -1)
last_updated_diff=$(($(date +%s)-$last_updated))
printf "%sUpdated %.*f seconds ago\n" "" 0 $last_updated_diff

echo "---"
echo ":chart_with_upwards_trend: Poloniex: | href=\"https://poloniex.com/exchange#btc_dash\""
percentChange=$(echo "$infoPoloniex" | grep -A1 percentChange | tail -1)
percentChange100=$(echo "$percentChange * 100" | bc -l)
[ $(echo $percentChange100 '>=' 0 | bc -l) -gt 0 ] && colorPoloniex="green" || colorPoloniex="red"
printf "%sPrice: %.*f BTC | color=$colorPoloniex\n" "* " 6 $(echo "$infoPoloniex" | grep -A1 last | tail -1)
printf "%sAsk: %.*f BTC\n" "* " 6 $(echo "$infoPoloniex" | grep -A1 lowestAsk | tail -1)
printf "%sBid: %.*f BTC\n" "* " 6 $(echo "$infoPoloniex" | grep -A1 highestBid | tail -1)
printf "%s24h Change: %.*f%% | color=$colorPoloniex\n" "* " 2 $percentChange100
printf "%s24h Volume: %.*f BTC\n" "* " 2 $(echo "$infoPoloniex" | grep -A1 baseVolume | tail -1)
quoteVolume=$(echo "$infoPoloniex" | grep -A1 quoteVolume | tail -1)
quoteVolumeK=$(echo "$quoteVolume / 1000" | bc -l)
printf "%s24h Volume: %.*fK DASH\n" "* " 2 $quoteVolumeK
printf "%s24h High: %.*f BTC | color=green\n" "* " 6 $(echo "$infoPoloniex" | grep -A1 high24hr | tail -1)
printf "%s24h Low: %.*f BTC | color=red\n" "* " 6 $(echo "$infoPoloniex" | grep -A1 low24hr | tail -1)
echo "---"
echo "Dash.org | href=\"https://www.dash.org\" image=$iconBase64"
echo "--Wallets | href=\"https://www.dash.org/downloads/\" image=$iconBase64"
echo "--Explorer | href=\"https://insight.dash.org/\" image=$iconBase64"
echo "--Forum | href=\"https://www.dash.org/forum/\" image=$iconBase64"
echo "--Docs | href=\"https://docs.dash.org/\" image=$iconBase64"
