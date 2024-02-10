#!/bin/bash
#
# <xbar.title>All-in-One ETH Price & GasNow Indicator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>take3315</xbar.author>
# <xbar.author.github>take3315</xbar.author.github>
# <xbar.desc>One line display to check the current ETH price, gasnow quick/fast/standard level in one go</xbar.desc>
# <xbar.image>https://imgur.com/a/525iUFo</xbar.image>
# <xbar.abouturl>https://beaconcha.in/gasnow</xbar.abouturl>


response=$(curl -s https://beaconcha.in/api/v1/execution/gasnow)
read code rapid fast standard slow timestamp price priceusd <<<${response//[^0-9]/ }
if ((fast > 60000000000)); then
    color="tomato"
elif ((fast < 30000000000)); then
    color="limegreen"
else
    color="gold"
fi
echo "\$$price ${rapid:0:((${#rapid} - 9))}/${fast:0:((${#fast} - 9))}/${standard:0:((${#standard} - 9))} | color=$color"