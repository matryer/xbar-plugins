#!/bin/bash
#
# <bitbar.title>Exchange Rates</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Mehmet Akif Tütüncü</bitbar.author>
# <bitbar.author.github>makiftutuncu</bitbar.author.github>
# <bitbar.image>https://github.com/makiftutuncu/exchange-bitbar/raw/master/Screenshot.png</bitbar.image>
# <bitbar.desc>This is a BitBar plugin for showing exchange rates of currencies in your menu bar. See https://github.com/makiftutuncu/exchange-bitbar#configuration for configuration.</bitbar.desc>
# <bitbar.dependencies>shell,curl,jq</bitbar.dependencies>
#
# Exchange Rates by Mehmet Akif Tütüncü
#
# This is a BitBar plugin for showing exchange rates of currencies in your menu bar.
# See https://github.com/makiftutuncu/exchange-bitbar#configuration for configuration.

export PATH="/usr/local/bin:$PATH"

# Currency pairs for which rates will show as array
CURRENCY_PAIRS=("USD-TRY" "EUR-TRY")

for pair in "${CURRENCY_PAIRS[@]}";
do
  URL=$(echo $pair | awk -F '-' '{print "https://akifs-exchange-api.herokuapp.com/rates?source="$1"&target="$2}')
  RESPONSE=$(curl -s -X GET $URL | jq -r '"\(.source),\(.target),\(.rate)"')
  echo $RESPONSE | awk -F ',' '{printf "💵 %s-%s: %0.3f\n", $1, $2, $3}'
done
