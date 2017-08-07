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

TIMESTAMP=$(date +%s)
REQUEST_METHOD="GET"
REQUEST_URL="https://api.coinbase.com"
REQUEST_PATH="/v2/accounts/${ACCOUNT_ID}"
SIGNATURE=$(echo -n "${TIMESTAMP}${REQUEST_METHOD}${REQUEST_PATH}" | openssl dgst -sha256 -hmac "${API_SECRET}")

echo -n "BTC: $(curl -s ${REQUEST_URL}${REQUEST_PATH} \
  --request ${REQUEST_METHOD} \
  --header "CB-VERSION: ${API_VERSION}" \
  --header "CB-ACCESS-KEY: ${API_KEY}" \
  --header "CB-ACCESS-SIGN: ${SIGNATURE}" \
  --header "CB-ACCESS-TIMESTAMP: ${TIMESTAMP}" | jq -r ".data.native_balance.amount")"
