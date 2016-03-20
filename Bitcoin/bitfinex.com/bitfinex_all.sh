#!/bin/bash

# Shows (Ask, Bid, Mid, High, Low, Volume and Timestamp from Bitfinex REST API Ticker BTCUSD 
#
# <bitbar.title>Bitfinex REST API Ticker BTCUSD plugin</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Roberto Santacroce Martins</bitbar.author>
# <bitbar.author.github>mileschet</bitbar.author.github>
# <bitbar.desc>Shows (Ask, Bid, Mid, High, Low, Volume and Timestamp from Bitfinex REST API Ticker BTCUSD </bitbar.desc>
# <bitbar.image>http://i.imgur.com/V8dABjz.png</bitbar.image>
#
# by Roberto Santacroce Martins
# Based on Coinbase bitbar plugin by Mat Ryer

#echo -n "Ƀ$"; 
RESULT=$(curl -s "https://api.bitfinex.com/v1/pubticker/BTCUSD") 
echo -n "Ƀ$ "; echo "$RESULT" | egrep -o '"last_price":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"last_price"://' | sed 's/\"//g'
echo -n "Ask "; echo "$RESULT" | egrep -o '"ask":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"ask"://' | sed 's/\"//g'
echo -n "Bid "; echo "$RESULT" | egrep -o '"bid":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"bid"://' | sed 's/\"//g'
echo -n "Low "; echo "$RESULT" | egrep -o '"low":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"low"://' | sed 's/\"//g'
echo -n "Mid "; echo "$RESULT" | egrep -o '"mid":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"mid"://' | sed 's/\"//g'
echo -n "High "; echo "$RESULT" | egrep -o '"high":"[0-9]+(\.)?([0-9]{0,2}")?' | sed 's/"high"://' | sed 's/\"//g'
echo -n "Vol "; echo "$RESULT" | egrep -o '"volume":"[0-9]+(\.)?([0-9]{0,8}")?' | sed 's/"volume"://' | sed 's/\"//g'
TIMESTAMP=$(echo "$RESULT" | egrep -o '"timestamp":"[0-9]+' | sed 's/"timestamp"://' | sed 's/\"//g')
echo -n "Updated At "; date -r "$TIMESTAMP" +"%F %T"
