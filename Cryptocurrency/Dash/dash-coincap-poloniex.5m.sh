#!/bin/bash

# <bitbar.title>Dash tickers: Coincap.io and Poloniex.com</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>UdjinM6</bitbar.author>
# <bitbar.author.github>UdjinM6</bitbar.author.github>
# <bitbar.desc>Shows the latest Dash info from Coincap.io and Poloniex.com</bitbar.desc>
# <bitbar.image>https://i.imgur.com/BRmoB48.png</bitbar.image>
# <bitbar.abouturl>https://www.dash.org/</bitbar.abouturl>

# To generate the image, grab PNG and increase the DPI from 72 to 144,
# then resize it to 32x32 and finally encode the image into base 64
# for example via https://base64.guru/converter/encode/image/png
iconBase64='iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4wsdAAwGzHGX5gAAAg5JREFUWMPt1z1oVEEQB/DfhRON+JXCmGKDIoiKmEIhIIhKCmsLwSKihb0EXikWChYi1ykIQggIFjaCglUghWJhLCwUFItYPPwKQvBiJDEhFnkHIu/d7TsuscnAcTA7uzPz35n/7GNd/rNUoqxq6QhGSpw7i3f4hQk8lYQveYbVyAMPYHfJ5A5l/+ezJF5mSUxKwmLDqCsi+y3Y2wG0B/ECd9XS7jIIbMbODl77JWxqINMVsWE79uB3i98CliKDGFZLT8Yi8B1J5qCZLKMbIauXY9if6fOK/RYGK6vSW7W0msF8EbebJLavsuqNXkuv42rOSh1DXWvANfcxV0ABfWsRwCymCmpmvvoXVGcwip4Sh8/jsCR8aMG2eVe9hB/VzPkRPGoju0lMR7Rxf45+AVONK7jcJrxvJGGmhU0PtuboFyXhWyOAC204r+NhhN2JAv3blUqspf34mrFZzPSs4xNGJWEiYs/ZAv3jRit8xkBWlTGyhAVJ+BnBAf04WrA6uhLAymicXqUWHCvQP5eEeuwwapcBb2CoYPVK2QdJGccHcQenCizG8ap5ALV0Q3ZAb4TLZWzEcZzOpmEzVrwpCXOtEOjDvTaeYa3kmiSMx7wJd2Bbh50PS8KDf5VFRdhbciY0kxTn8pw3Q2CgA46f4QnGJKGwzYsC2IX3JdhxJqvs1/iY7a1Lwuz6p9e6tJI/DAh9aD+iV7oAAAAASUVORK5CYII='

# Grab all info beforehand
token_info_poloniex=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep BTC_DASH | tr -d '{}"' | tr ':,' '\n')
token_info_coincap=$(curl -s https://api.coincap.io/v2/assets/dash/ | tr -d '{}" ' | tr ':,' '\n')
btc_info_coincap=$(curl -s https://api.coincap.io/v2/assets/bitcoin/ | tr -d '{}" ' | tr ':,' '\n')

# Menu bar
token_price_btc=$(echo "$token_info_poloniex" | grep -A1 last | tail -1)
token_price_usd=$(echo "$token_info_coincap" | grep -A1 priceUsd | tail -1)
token_price_btc_precision=6
if (( $(echo "$token_price_usd >= 100" | bc -l) )); then token_price_usd_precision="$((token_price_btc_precision-3))"; else token_price_usd_precision="$((token_price_btc_precision-2))"; fi
printf "%.*f | dropdown=false image=%s\n" "$token_price_btc_precision" "$token_price_btc" "$iconBase64"
printf "$%.*f | dropdown=false image=%s\n" "$token_price_usd_precision" "$token_price_usd" "$iconBase64"

# BTC
btc_percent_change_24h=$(echo "$btc_info_coincap" | grep -A1 changePercent24Hr | tail -1)
if (( $(echo "$btc_percent_change_24h >= 0" | bc -l) )); then btc_color_coincap="green"; else btc_color_coincap="red"; fi
btc_usd_price=$(echo "$btc_info_coincap" | grep -A1 priceUsd | tail -1)
echo "---"
printf ":moneybag: BTC: $%.*f | color=$btc_color_coincap href=\"http://coincap.io/assets/bitcoin/\"\n" 2 "$btc_usd_price"

# Coincap.io
token_percent_change_24h=$(echo "$token_info_coincap" | grep -A1 changePercent24Hr | tail -1)
if (( $(echo "$token_percent_change_24h >= 0" | bc -l) )); then token_color_coincap="green"; else token_color_coincap="red"; fi
token_rank=$(echo "$token_info_coincap" | grep -A1 rank | tail -1)
token_available_supply=$(echo "$token_info_coincap" | grep -A1 supply | tail -1)
token_available_supply_mln=$(echo "$token_available_supply / 1000000" | bc -l)
token_market_cap_usd=$(echo "$token_info_coincap" | grep -A1 marketCapUsd | tail -1)
token_market_cap_usd_mln=$(echo "$token_market_cap_usd / 1000000" | bc -l)
token_volume_usd=$(echo "$token_info_coincap" | grep -A1 volumeUsd24Hr | tail -1)
token_volume_usd_m=$(echo "$token_volume_usd / 1000000" | bc -l)
echo "---"
echo ":chart_with_upwards_trend: Coincap.io: | href=\"http://coincap.io/assets/dash/\""
printf "%sRank: %.*f\n" "* " 0 "$token_rank"
printf "%sSupply: %.*fM DASH\n" "* " 2 "$token_available_supply_mln"
printf "%sMarket Cap: $%.*fM | color=$token_color_coincap\n" "* " 2 "$token_market_cap_usd_mln"
printf "%sPrice: $%.*f | color=$token_color_coincap\n" "* " 2 "$token_price_usd"
printf "%s24h Change: %.*f%% | color=$token_color_coincap\n" "* " 2 "$token_percent_change_24h"
printf "%s24h Volume: $%.*fM\n" "* " 2 "$token_volume_usd_m"

# Poloniex.com
token_percent_change=$(echo "$token_info_poloniex" | grep -A1 percentChange | tail -1)
token_percent_change=$( echo "$token_percent_change * 100" | bc -l )
if (( $(echo "$token_percent_change >= 0" | bc -l) )); then token_color_poloniex="green"; else token_color_poloniex="red"; fi
token_lowest_ask=$(echo "$token_info_poloniex" | grep -A1 lowestAsk | tail -1)
token_highest_bid=$(echo "$token_info_poloniex" | grep -A1 highestBid | tail -1)
token_base_volume=$(echo "$token_info_poloniex" | grep -A1 baseVolume | tail -1)
token_quote_volume=$(echo "$token_info_poloniex" | grep -A1 quoteVolume | tail -1)
token_quote_volume_k=$(echo "$token_quote_volume / 1000" | bc -l)
token_high_24h=$(echo "$token_info_poloniex" | grep -A1 high24hr | tail -1)
token_low_24h=$(echo "$token_info_poloniex" | grep -A1 low24hr | tail -1)
echo "---"
echo ":chart_with_upwards_trend: Poloniex.com: | href=\"https://poloniex.com/exchange#btc_dash\""
printf "%sPrice: %.*f BTC | color=$token_color_poloniex\n" "* " 6 "$token_price_btc"
printf "%sAsk: %.*f BTC\n" "* " 6 "$token_lowest_ask"
printf "%sBid: %.*f BTC\n" "* " 6 "$token_highest_bid"
printf "%s24h Change: %.*f%% | color=$token_color_poloniex\n" "* " 2 "$token_percent_change"
printf "%s24h Volume: %.*f BTC\n" "* " 2 "$token_base_volume"
printf "%s24h Volume: %.*fK DASH\n" "* " 2 "$token_quote_volume_k"
printf "%s24h High: %.*f BTC | color=green\n" "* " 6 "$token_high_24h"
printf "%s24h Low: %.*f BTC | color=red\n" "* " 6 "$token_low_24h"

# URLs
echo "---"
echo "Dash.org | href=\"https://www.dash.org\" image=$iconBase64"
echo "--Wallets | href=\"https://www.dash.org/downloads/\" image=$iconBase64"
echo "--Explorer | href=\"https://explorer.dash.org/\" image=$iconBase64"
echo "--Forum | href=\"https://www.dash.org/forum/\" image=$iconBase64"
echo "--Docs | href=\"https://docs.dash.org/\" image=$iconBase64"
