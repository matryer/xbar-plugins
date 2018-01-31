#!/bin/bash

# <bitbar.title>Garlicoin Balance</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>ferdizz</bitbar.author>
# <bitbar.author.github>ferdizz</bitbar.author.github>
# <bitbar.desc>Shows your current Garlicoin balance.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/ferdizz/files/master/garlic-screenshot.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>

balance=0

declare -a addresses=(
    "INSERT-YOUR-WALLET-ADDRESS-#1-HERE"
    "INSERT-YOUR-WALLET-ADDRESS-#2-HERE" # <-- Repeat for every address you want to add
)

for i in "${addresses[@]}"
do
    res="$(curl -s 'https://garli.co.in/ext/getbalance/'"$i")"
    balance="$(echo "$balance + $res" | bc)"
done

echo "$(printf '%.1f' "$balance") ₲"
echo "---"
echo "$balance Garlicoins"

# GPuEScAvXJvviVMaTH2xg4KPgPDD52AK1M if you want to donate some of that sweet sweet garlic