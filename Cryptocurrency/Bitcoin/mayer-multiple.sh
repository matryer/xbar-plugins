#!/bin/bash

# <bitbar.title>Mayer Multiple (BTC)</bitbar.title>
# <bitbar.author>Tadek Teleżyński</bitbar.author>
# <bitbar.author.github>tadeoos</bitbar.author.github>
# <bitbar.desc>
#   Displays Mayer Multiple for Bitcoin
# </bitbar.desc>
# <bitbar.image>https://i.imgur.com/NsOofDJ.png</bitbar.image>

RESULT=$(curl -s "https://mayermultiple.info/current.json")
MM=$(echo "$RESULT" | grep -E -o '"current_mayer_multiple": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"current_mayer_multiple"://' | sed 's/\"//g')
BTC=$(echo "$RESULT" | grep -E -o '"btc_price": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"btc_price"://' | sed 's/\"//g')
AMM=$(echo "$RESULT" | grep -E -o '"average_mayer_multiple": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"average_mayer_multiple"://' | sed 's/\"//g')
PTH=$(echo "$RESULT" | grep -E -o '"percentage_time_higher": "[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"percentage_time_higher"://' | sed 's/\"//g')

if (( $(echo "$MM < 2.4" |bc -l) )); then
	echo "$MM | color='green'";
else
	echo "$MM";
fi
echo "---"
echo -n "BTC $"; echo "$BTC"
echo -n "Average Mayer Multiple is "; echo "$AMM"
echo -n "Mayer Multiple has historically been higher "; echo -n "$PTH"; echo "% of the time";
