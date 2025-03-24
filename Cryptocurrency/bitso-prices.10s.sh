#!/usr/bin/env bash

# <xbar.title>Bitso Price (USD / MXN)</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alex Ventura</xbar.author>
# <xbar.author.github>alexventuraio</xbar.author.github>
# <xbar.desc>Displays BTC, LTC, ETH, XRP and BCH prices from Bitso either in USD or MXN</xbar.desc>
# <xbar.image>https://i.ibb.co/ZRFdnxpC/Screenshot-2025-03-24-at-14-25-12.png</xbar.image>
# <xbar.dependencies>curl, bash, jq</xbar.dependencies>

# If you find this small script someway useful, please consider a tip or buying me a coffee:
# Bitcoin: 3DUzWfDWR75AHuwnZcF7KyQAHLXf3iu7YV
# Litecoin: MLjstS6a4a4AgXSd2cLhznPNjDhF5sucmr

export LANG='en_US.UTF-8'
export LC_NUMERIC='en_US'
export PATH="/usr/local/bin/:$PATH"
export PATH=/opt/homebrew/bin:$PATH

# Show prices in USD or MXN
SHOW_PRICES_IN_USD="true"

# Fetch data
BTC_JSON_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=btc_mxn')
ETH_JSON_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=eth_mxn')
XRP_JSON_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=xrp_mxn')
BCH_JSON_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=bch_mxn')
LTC_JSON_MXN=$(curl -s 'https://api.bitso.com/v3/ticker/?book=ltc_mxn')

BTC_JSON_USD=$(curl -s 'https://api.bitso.com/v3/ticker/?book=btc_usd')
ETH_JSON_USD=$(curl -s 'https://api.bitso.com/v3/ticker/?book=eth_usd')
XRP_JSON_USD=$(curl -s 'https://api.bitso.com/v3/ticker/?book=xrp_usd')
BCH_JSON_USD=$(curl -s 'https://api.bitso.com/v3/ticker/?book=bch_usd')
LTC_JSON_USD=$(curl -s 'https://api.bitso.com/v3/ticker/?book=ltc_usd')

# Select currency
if [[ "$SHOW_PRICES_IN_USD" == "true" ]]; then
  CURRENCY="USD"
  BTC_JSON_DATA="$BTC_JSON_USD"
  ETH_JSON_DATA="$ETH_JSON_USD"
  XRP_JSON_DATA="$XRP_JSON_USD"
  BCH_JSON_DATA="$BCH_JSON_USD"
  LTC_JSON_DATA="$LTC_JSON_USD"
else
  CURRENCY="MXN"
  BTC_JSON_DATA="$BTC_JSON_MXN"
  ETH_JSON_DATA="$ETH_JSON_MXN"
  XRP_JSON_DATA="$XRP_JSON_MXN"
  BCH_JSON_DATA="$BCH_JSON_MXN"
  LTC_JSON_DATA="$LTC_JSON_MXN"
fi

# Extract last prices
BTC_NOW=$(echo "$BTC_JSON_DATA" | jq -r '.payload.last')
ETH_NOW=$(echo "$ETH_JSON_DATA" | jq -r '.payload.last')
XRP_NOW=$(echo "$XRP_JSON_DATA" | jq -r '.payload.last')
BCH_NOW=$(echo "$BCH_JSON_DATA" | jq -r '.payload.last')
LTC_NOW=$(echo "$LTC_JSON_DATA" | jq -r '.payload.last')

# Display prices
printf "BTC: %'.0f %s | color=white \n" "$BTC_NOW" "$CURRENCY"

echo '---'

printf "1 BTC = %'.2f %s  | color=green \n" "$BTC_NOW" "$CURRENCY"

echo '---'

# Display high, low, ask, bid for BTC
printf "HIGH: \t$ %'.2f %s\n" "$(echo "$BTC_JSON_DATA" | jq -r '.payload.high')" "$CURRENCY"
printf "LOW:  \t$ %'.2f %s\n" "$(echo "$BTC_JSON_DATA" | jq -r '.payload.low')" "$CURRENCY"
printf "ASK:  \t$ %'.2f %s\n" "$(echo "$BTC_JSON_DATA" | jq -r '.payload.ask')" "$CURRENCY"
printf "BID:  \t$ %'.2f %s\n" "$(echo "$BTC_JSON_DATA" | jq -r '.payload.bid')" "$CURRENCY"

echo '---'

printf "1 LTC = %'.2f %s  | color=orange \n" "$LTC_NOW" "$CURRENCY"

echo '---'

# Display high, low, ask, bid for LTC
printf "HIGH: \t$ %'.2f %s\n" "$(echo "$LTC_JSON_DATA" | jq -r '.payload.high')" "$CURRENCY"
printf "LOW:  \t$ %'.2f %s\n" "$(echo "$LTC_JSON_DATA" | jq -r '.payload.low')" "$CURRENCY"
printf "ASK:  \t$ %'.2f %s\n" "$(echo "$LTC_JSON_DATA" | jq -r '.payload.ask')" "$CURRENCY"
printf "BID:  \t$ %'.2f %s\n" "$(echo "$LTC_JSON_DATA" | jq -r '.payload.bid')" "$CURRENCY"

echo '---'

printf "1 ETH = %'.2f %s  | color=blue \n" "$ETH_NOW" "$CURRENCY"a

echo '---'

# Display high, low, ask, bid for ETH
printf "HIGH: \t$ %'.2f %s\n" "$(echo "$ETH_JSON_DATA" | jq -r '.payload.high')" "$CURRENCY"
printf "LOW:  \t$ %'.2f %s\n" "$(echo "$ETH_JSON_DATA" | jq -r '.payload.low')" "$CURRENCY"
printf "ASK:  \t$ %'.2f %s\n" "$(echo "$ETH_JSON_DATA" | jq -r '.payload.ask')" "$CURRENCY"
printf "BID:  \t$ %'.2f %s\n" "$(echo "$ETH_JSON_DATA" | jq -r '.payload.bid')" "$CURRENCY"

echo '---'

printf "1 XRP = %'.2f %s  | color=red \n" "$XRP_NOW" "$CURRENCY"

echo '---'

# Display high, low, ask, bid for XRP
printf "HIGH: \t$ %'.2f %s\n" "$(echo "$XRP_JSON_DATA" | jq -r '.payload.high')" "$CURRENCY"
printf "LOW:  \t$ %'.2f %s\n" "$(echo "$XRP_JSON_DATA" | jq -r '.payload.low')" "$CURRENCY"
printf "ASK:  \t$ %'.2f %s\n" "$(echo "$XRP_JSON_DATA" | jq -r '.payload.ask')" "$CURRENCY"
printf "BID:  \t$ %'.2f %s\n" "$(echo "$XRP_JSON_DATA" | jq -r '.payload.bid')" "$CURRENCY"

echo '---'

printf "1 BCH = %'.2f %s  | color=purple \n" "$BCH_NOW" "$CURRENCY"

echo '---'

# Display high, low, ask, bid for BCH
printf "HIGH: \t$ %'.2f %s\n" "$(echo "$BCH_JSON_DATA" | jq -r '.payload.high')" "$CURRENCY"
printf "LOW:  \t$ %'.2f %s\n" "$(echo "$BCH_JSON_DATA" | jq -r '.payload.low')" "$CURRENCY"
printf "ASK:  \t$ %'.2f %s\n" "$(echo "$BCH_JSON_DATA" | jq -r '.payload.ask')" "$CURRENCY"
printf "BID:  \t$ %'.2f %s\n" "$(echo "$BCH_JSON_DATA" | jq -r '.payload.bid')" "$CURRENCY"
