#!/bin/bash
#
# <xbar.title>Exchange Rates</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Mehmet Akif Tütüncü</xbar.author>
# <xbar.author.github>makiftutuncu</xbar.author.github>
# <xbar.image>https://github.com/makiftutuncu/exchange-bitbar/raw/master/Screenshot.png</xbar.image>
# <xbar.desc>This is a BitBar plugin for showing exchange rates of currencies in your menu bar. See https://github.com/makiftutuncu/exchange-bitbar#configuration for configuration.</xbar.desc>
# <xbar.dependencies>shell,curl,jq</xbar.dependencies>
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
