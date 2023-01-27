#!/bin/bash

# Shows latest Bitcoin network fees on mempool.space (slow, medium, and fast via dropdown), and allows the user to select a default view for the bar itself. 
# If you like the plugin, you can send me a few sats via Lightning Network to tips@lightning.jle.vi
# <xbar.title>Mempool Bitcoin Fees Estimator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Jonathan A Levi</xbar.author>
# <xbar.author.github>entreprenewer</xbar.author.github>
# <xbar.desc>Shows latest Bitcoin network fees on mempool.space (slow, medium, and fast via dropdown), and allows the user to select a default view for the bar itself. </xbar.desc>
# <xbar.image>https://i.imgur.com/sJAnweh.png</xbar.image>
#
# by Jonathan A Levi
# Based on Bitfinex bitbar plugin by Roberto Santacroce Martins and Dash tickers: Coincap.io and Poloniex.com by UdjinM6

# Grab all info beforehand
fee_info=$(curl -s "https://mempool.space/api/v1/fees/recommended")
fast=$(curl -s "https://mempool.space/api/v1/fees/recommended" | egrep -o '"fastestFee":[0-9]+(\.)?([0-9]{0,2})?' | sed 's/"fastestFee"://')
medium=$(curl -s "https://mempool.space/api/v1/fees/recommended" | egrep -o '"halfHourFee":[0-9]+(\.)?([0-9]{0,2})?' | sed 's/"halfHourFee"://')
slow=$(curl -s "https://mempool.space/api/v1/fees/recommended" | egrep -o '"hourFee":[0-9]+(\.)?([0-9]{0,2})?' | sed 's/"hourFee"://')

# SET DEFAULT VIEW HERE
# (Replace with $slow, $medium, or $fast)
default=$slow


#Menu bar
printf "%ld sat/vB\n" $default
echo "---"
printf "Slow: %ld sat/vB | color=red\n" $slow
printf "Medium: %ld sat/vB | color=orange\n" $medium
printf "Fast: %ld sat/vB | color=green\n" $fast
echo "Visit Mempool | href=\"https://mempool.space\""
