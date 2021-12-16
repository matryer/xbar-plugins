#!/bin/bash

# <xbar.title>Garlicoin Balance</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>ferdizz</xbar.author>
# <xbar.author.github>ferdizz</xbar.author.github>
# <xbar.desc>Shows your current Garlicoin balance.</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/ferdizz/files/master/garlic-screenshot.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

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