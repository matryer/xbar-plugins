#!/bin/bash

# <bitbar.title>Kraken.com ETHEUR last price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>G.G.</bitbar.author>
# <bitbar.author.github>ggrelet</bitbar.author.github>
# <bitbar.desc>Gives the last price of ether to euro from Kraken.com</bitbar.desc>
# <bitbar.image>https://i.imgur.com/iGX2yjR.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>

val=$(curl -s "https://api.kraken.com/0/public/Ticker?pair=ETHEUR" | tr -d '{}"[]' | tr ':,' '\n' | grep -A1 "^c$" | tail -1)

echo "$(printf "𝚵 %.3f \n" "$val") | size=13"
echo "---"
echo "Kraken.com | href=\"https://www.kraken.com/\""
