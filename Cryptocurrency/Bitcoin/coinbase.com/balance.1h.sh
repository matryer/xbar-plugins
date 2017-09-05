#!/bin/bash
# <bitbar.title>Coinbase.com Your Balance</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Mat Ryer</bitbar.author>
# <bitbar.author.github>matryer</bitbar.author.github>
# <bitbar.desc>Shows your balance in BTC.  Be sure you add your API & account details.</bitbar.desc>

API_KEY="YOUR_API_KEY"
API_SECRET="YOUR_API_SECRET"
ACCOUNT_ID="YOUR_ACCOUNT_ID"
API_VERSION="2017-08-07" # You'll need to verify this in your Account Details

REQUEST_METHOD="GET"
REQUEST_URL="https://api.coinbase.com"
REQUEST_PATH="/v2/accounts/${ACCOUNT_ID}"

TIMESTAMP=$(date +%s)
SIGNATURE=$(echo -n "${TIMESTAMP}${REQUEST_METHOD}${REQUEST_PATH}" | openssl dgst -sha256 -hmac "${API_SECRET}")

BTC_BALANCE=$(curl -s ${REQUEST_URL}${REQUEST_PATH} \
  --request ${REQUEST_METHOD} \
  --header "CB-VERSION: ${API_VERSION}" \
  --header "CB-ACCESS-KEY: ${API_KEY}" \
  --header "CB-ACCESS-SIGN: ${SIGNATURE}" \
  --header "CB-ACCESS-TIMESTAMP: ${TIMESTAMP}" | jq -r ".data.balance.amount")

LAST_BTC_RATE=$(cat ~/.bitbar_last_btc_rate)
LAST_USD_BALANCE=$(cat ~/.bitbar_last_usd_balance)

BTC_RATE=$(curl -s https://api.coinbase.com/v2/prices/BTC-USD/spot | jq -r '.data.amount')
USD_BALANCE=$(echo "$BTC_BALANCE * $BTC_RATE" | bc)

DELTA_USD=$(printf "%.2f" "$(echo "$USD_BALANCE-${LAST_USD_BALANCE:-0}" | bc)")
DELTA_BTC=$(printf "%.2f" "$(echo "$BTC_RATE-${LAST_BTC_RATE:-0}" | bc)")

if [ "$(echo "$DELTA_BTC > 0.00" | bc)" -eq "1" ]; then
  HIDE_DELTA=0
  CHG_SYMBOL="▲ "
  COLOR=green
elif [ "$(echo "$DELTA_BTC < 0.00" | bc)" -eq "1" ]; then
  HIDE_DELTA=0
  CHG_SYMBOL="▼ "
  COLOR=red
  DELTA_BTC=${DELTA_BTC#-}
  DELTA_USD=${DELTA_USD#-}
else
  HIDE_DELTA=1
fi

echo -n "$BTC_RATE" > ~/.bitbar_last_btc_rate
echo -n "$USD_BALANCE" > ~/.bitbar_last_usd_balance

if [ "$HIDE_DELTA" -eq "1" ]; then
  printf "Ƀ%.2f\n" "${BTC_RATE}"
  printf "$%.2f\n" "${USD_BALANCE}"
else
  printf "Ƀ%.2f (${CHG_SYMBOL}%.2f) | color=$COLOR\n" "${BTC_RATE}" "${DELTA_BTC}"
  printf "$%.2f (${CHG_SYMBOL}%.2f) | color=$COLOR\n" "${USD_BALANCE}" "${DELTA_USD}"
fi
echo "---"
printf "Balance: %f (%.2f)" "${BTC_BALANCE}" "${USD_BALANCE}"
