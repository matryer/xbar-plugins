#!/usr/bin/env bash

# Choose a coin
function choose() {
  osascript <<EOT
    choose from list {"BTC", "LTC", "ETH"} with title "BitBar: GDAX" with prompt "Choose a coin" default items "LTC"
EOT
}

product=$(choose)
if [[ ! $product == "false" ]]; then
  echo "{\"product\": \""$product"-USD\"}" > /Users/f/Documents/BitBar/gdax/gdax_settings.json;
fi
