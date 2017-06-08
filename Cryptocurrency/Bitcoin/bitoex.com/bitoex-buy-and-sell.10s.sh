#!/bin/bash
# <bitbar.title>BitoEX Buy and Sell</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ryan Chen</bitbar.author>
# <bitbar.author.github>ryanchentw</bitbar.author.github>
# <bitbar.desc>Show latest buy and seller values in TWD from BitoEX. BitoEX is a popular BTC exchange in TW https://www.bitoex.com</bitbar.desc>


regexp='"([0-9,]+)","([0-9,]+)"'
resp=$(curl -s https://www.bitoex.com/sync/dashboard_fixed/"$(date +%s)")

if [[ $resp =~ $regexp ]]; then
    echo Buy: NT$ "${BASH_REMATCH[1]}"
    echo Sell: NT$ "${BASH_REMATCH[2]}"
    echo '---'
    echo 'Go chart | href=https://www.bitoex.com/charts?locale=zh-tw'
fi
