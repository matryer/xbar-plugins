#!/bin/bash
#
# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>GasRN</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>the-decentraliizer</xbar.author>
#  <xbar.author.github>the-decentralizer</xbar.author.github>
#  <xbar.desc>Grabs and parses data from https://etherchain.org/api/gasnow</xbar.desc>
#  <xbar.dependencies>bash</xbar.dependencies>


response=$(curl -s 'https://etherchain.org/api/gasnow')
read code rapid fast standard slow timestamp priceUSD <<<${response//[^0-9]/ }
echo "Ξ Rapid ${rapid:0:((${#rapid} - 9))} | color=green" 
echo "---"
echo "Ξ Fast ${fast:0:((${#fast} - 9))} | color=orange"
echo "---"
echo "Ξ Standard ${standard:0:((${#standard} - 9))} | color=blue"
echo "---"
echo "Ξ Slow ${slow:0:((${#slow} - 9))} | color=purple"
echo "---"
# using long form for users that might not have the bash 4.0+
echo "Ξ Price \$${priceUSD::${#priceUSD}-3} | color=red"




 