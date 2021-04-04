#!/bin/bash

# <xbar.title>Kraken.com ETHEUR last price</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>G.G.</xbar.author>
# <xbar.author.github>ggrelet</xbar.author.github>
# <xbar.desc>Gives the last price of ether to euro from Kraken.com</xbar.desc>
# <xbar.image>https://i.imgur.com/iGX2yjR.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

val=$(curl -s "https://api.kraken.com/0/public/Ticker?pair=ETHEUR" | tr -d '{}"[]' | tr ':,' '\n' | grep -A1 "^c$" | tail -1)

echo "$(printf "𝚵 %.3f \n" "$val") | size=13"
echo "---"
echo "Kraken.com | href=\"https://www.kraken.com/\""
