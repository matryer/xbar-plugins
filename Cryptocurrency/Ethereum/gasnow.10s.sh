#!/bin/bash
#
# <bitbar.title>ETH Gas Price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Biran Yucel</bitbar.author>
# <bitbar.author.github>biranyucel</bitbar.author.github>
# <bitbar.desc>ETH GasPrice forecast system.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/f1hPKO2.png</bitbar.image>
# <bitbar.abouturl>https://www.gasnow.org/</bitbar.abouturl>
#
#ETH GasPrice forecast system based on SparkPool Pending Transaction Mempool

response=$(curl -s https://www.gasnow.org/api/v3/gas/price\?utm_source\=:)
read code rapid fast standard slow timestamp <<<${response//[^0-9]/ }
echo "𝚵 Rapid ${rapid:0:((${#rapid} - 9))} | color=green" 
echo "𝚵 Fast ${fast:0:((${#fast} - 9))} | color=orange"
echo "𝚵 Standard ${standard:0:((${#standard} - 9))} | color=blue"
echo "𝚵 Slow ${slow:0:((${#slow} - 9))} | color=purple"