#!/bin/bash
#
# <xbar.title>ETH Gas Price</xbar.title>
# <xbar.version>v1.0.1</xbar.version>
# <xbar.author>Biran Yucel</xbar.author>
# <xbar.author.github>biranyucel</xbar.author.github>
# <xbar.desc>ETH GasPrice forecast system.</xbar.desc>
# <xbar.image>https://i.imgur.com/f1hPKO2.png</xbar.image>
# <xbar.abouturl>https://www.gasnow.org/</xbar.abouturl>
#
#ETH GasPrice forecast system based on SparkPool Pending Transaction Mempool

response=$(curl -s https://etherchain.org/api/gasnow)
read code rapid fast standard slow timestamp <<<${response//[^0-9]/ }
echo "𝚵 Rapid ${rapid:0:((${#rapid} - 9))} | color=green" 
echo "𝚵 Fast ${fast:0:((${#fast} - 9))} | color=orange"
echo "𝚵 Standard ${standard:0:((${#standard} - 9))} | color=blue"
echo "𝚵 Slow ${slow:0:((${#slow} - 9))} | color=purple"
