#!/bin/bash

# <bitbar.title>Garlicoin Balance</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>ferdizz</bitbar.author>
# <bitbar.author.github>ferdizz</bitbar.author.github>
# <bitbar.desc>Shows your current Garlicoin balance.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/ferdizz/files/master/garlic-screenshot.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
 
address="INSERT-YOUR-WALLET-ADDRESS-HERE"
res="$(curl -s 'https://explorer.grlc-bakery.fun/ext/getbalance/'"$address")"

echo "$(printf '%.1f' "$res") ₲"
echo "---"
echo "$res Garlicoins"

# GPuEScAvXJvviVMaTH2xg4KPgPDD52AK1M if you want to donate some of that sweet sweet garlic