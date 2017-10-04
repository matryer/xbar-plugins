#!/bin/bash
#
# yahoo stock info plugin
# much simpler than stock plugin, no API key required
# by http://srinivas.gs
# 
# <bitbar.title>Stock price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Srinivas Gorur-Shandilya</bitbar.author>
# <bitbar.author.github>sg-s</bitbar.author.github>
# <bitbar.desc>Customizable stock price display </bitbar.desc>


# specify which stocks you want to monitor here
stock[0]="GOOG"
stock[1]="AAPL"
stock[2]="AMZN"

# this function is to shuffle the elements of an array in bash
shuffle() {
   local i tmp size max rand
   size=${#stock[*]}
   max=$(( 32768 / size * size ))
   for ((i=size-1; i>0; i--)); do
      while (( (rand=RANDOM) >= max )); do :; done
      rand=$(( rand % (i+1) ))
      tmp=${stock[i]} stock[i]=${stock[rand]} stock[rand]=$tmp
   done
}
shuffle

# we get stock quotes from Yahoo
s='http://download.finance.yahoo.com/d/quotes.csv?s=stock_symbol&f=l1'

n=${#stock[@]}
n=$((n-1))

for (( c=0; c<=n; c++ ))
do
	echo -n "${stock[$c]}:"; curl -s "${s/stock_symbol/${stock[$c]}}"

   # experimental: also log to disk
   # echo -n $(date) >> stock_history.txt;  echo -n "  " >> stock_history.txt; echo -n ${stock[$c]} >> stock_history.txt; echo -n ":" >> stock_history.txt; curl -s echo ${s/stock_symbol/${stock[$c]}} >> stock_history.txt

done
