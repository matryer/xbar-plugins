#!/bin/bash

# <bitbar.title>Kraken.com ETHEUR last price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>G.G.</bitbar.author>
# <bitbar.author.github>ggrelet</bitbar.author.github>
# <bitbar.desc>Gives the last price of ether to euro from Kraken.com</bitbar.desc>
# <bitbar.image>http://www.hosted-somewhere/pluginimage</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>http://url-to-about.com/</bitbar.abouturl>

val=$(curl -s "https://api.kraken.com/0/public/Ticker?pair=ETHEUR" | tr -d '{}"[]' | tr ':,' '\n' | grep -A1 "^c$" | tail -1)

echo "$(printf "loΞ %.3f \n" "$val") | size=13"
echo "---"
echo "Kraken.com | href="https://www.kraken.com/""