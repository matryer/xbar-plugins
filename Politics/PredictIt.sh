#!/bin/bash
#Todd Houle
#Jun2016

#Instructions: At the bottom of the script, add a getData line with the contract or market you'd like to follow. You can get the contract name by looking at the PredictIt page and entering they 'symbol' on that page end the end of this script.

# <bitbar.title>Political Outcome Predictions</bitbar.title> 
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Todd Houle</bitbar.author>
# <bitbar.image>http://i.imgur.com/WVAdSsD.png</bitbar.image>
# <bitbar.author.github>tmhoule</bitbar.author.github>
# <bitbar.desc>Displays predictions from PredictIt (who will win elections,etc.) No API key or accounts needed. Watch Trumps chance of winning the election right from your menu bar.</bitbar.desc>

#Show images in menu.  Comment out one of the lines below.
useImages=true
#useImages=false                                                                                                                                                                                                                                                                 

#must be this percent or higher to show in menu
cutoff=5

echo "PI"
echo "---"

getData () {
    market=$1
    curl -H "Accept: application/xml" -kfs "https://www.predictit.org/api/marketdata/ticker/$market" -X GET  | xmllint --format - > "/private/tmp/$market.$$.xml" 2>/dev/null
    Description=$(xpath "/private/tmp/$market.$$.xml" 'MarketData/Name' 2>/dev/null| awk -F\> '{print $2}'|awk -F\< '{print $1}' 2>/dev/null)
    echo "$Description"
    IDsInPol=$(xpath "/private/tmp/$market.$$.xml" 'MarketData/Contracts' 2>/dev/null|grep -i "<ID>"| awk -F\> '{print $2}'|awk -F\< '{print $1}' 2>/dev/null)
    PolArry=($IDsInPol)
    arrayCount=${#PolArry[@]}
    for onPol in "${PolArry[@]}"; do
        name=$(xmllint --xpath "string(//Contracts/MarketContract[ID=$onPol]/Name)" "/tmp/$market.$$.xml")
        curPriceCents=$(xmllint --xpath "string(//Contracts/MarketContract[ID=$onPol]/LastTradePrice)" "/tmp/$market.$$.xml")
        curPriceraw=$(echo "$curPriceCents * 100" | bc -l|awk -F. '{print $1}')
        if [ "$curPriceraw" -ge "$cutoff" ] || [ "$arrayCount" = "1" ]; then
            curPrice=$curPriceraw"%"
            imgURL=$(xmllint --xpath "string(//Contracts/MarketContract[ID=$onPol]/Image)" "/tmp/$market.$$.xml" 2>/dev/null)
            if [ "$useImages" = "true" ]; then
                img=$(curl -kfs "$imgURL"|base64)
            fi
            linkURL=$(xmllint --xpath "string(//Contracts/MarketContract[ID=$onPol]/URL)" "/tmp/$market.$$.xml" 2>/dev/null)
            echo "--$name - $curPrice |image=$img color=black href=$linkURL"
        fi
    done
    rm "/private/tmp/$market.$$.xml"
}

getData DNOM16
getData RNOM16
getData USPREZ16
