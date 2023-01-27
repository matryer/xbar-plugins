#!/bin/bash
#
# <xbar.title>ETH Gas Price</xbar.title>
# <xbar.version>v2.0.1</xbar.version>
# <xbar.author>Biran Yucel</xbar.author>
# <xbar.author.github>biranyucel</xbar.author.github>
# <xbar.desc>ETH GasPrice forecast system.</xbar.desc>
# <xbar.image>https://i.imgur.com/f1hPKO2.png</xbar.image>
# <xbar.abouturl>https://ethgasstation.info/</xbar.abouturl>
#
#ETH GasPrice forecast system based on SparkPool Pending Transaction Mempool

response=$(curl -s https://ethgasstation.info/api/ethgasAPI.json\?)
read fast fastest safeLow average else <<<${response//[^0-9]/ }
echo "𝚵 Rapid ${fastest:0:((${#fastest} - 1))} | color=green"
echo "𝚵 Fast ${fast:0:((${#fast} - 1))} | color=orange"
echo "𝚵 Standard ${average:0:((${#average} - 1))} | color=blue"
echo "𝚵 Slow ${safeLow:0:((${#safeLow} - 1))} | color=purple"