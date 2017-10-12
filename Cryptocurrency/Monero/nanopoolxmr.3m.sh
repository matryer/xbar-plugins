#!/bin/bash

# <bitbar.title>Monero Nanopool Monitor</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>FF</bitbar.author>
# <bitbar.author.github>ffcomtr</bitbar.author.github>
# <bitbar.desc>Shows your Monero mining status at Nanopool</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/ffcomtr/Bitbar.xmr.nanopool/master/bitbarnanopool.png</bitbar.image>
# <bitbar.abouturl>https://github.com/ffcomtr</bitbar.abouturl>
#
# Please change wallet string to your wallet to monitor your account. Jq is required. Install: brew install jq
#

moneroIconBase64='iVBORw0KGgoAAAANSUhEUgAAABoAAAAeCAYAAAAy2w7YAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAE3RFWHRTb2Z0d2FyZQBtb25lcm8uaG9398/iYQAAAyxJREFUeNrclstrE1EUxu9MJkWrtvWxKmgXRrG4KBRELFUqVtFKqnUnLkRF1L9AwYXBpeBCEVcuXIgbQRG0rYraGokP4iok5KHZqJi0TUwnD9I8/U48I9Mwk7FNcOGBX7i599zz3ee5I7lcLmFh/eAgGAAOsA5IIAm+AA+YBN5GQZQGbXvBJTAMZIP2DWArOASuglfgGnhuFMwowApwG7wEB0x86o1muA88A3fAKiuhTvAYXODOy7Ez4AlYbya0EjzgWTRrQ+AhWG0kdB3sF62zPeBWvdBucJ7L0+BrEwI/wGsun9QGrwld0e2JGxwGsWWIJIATvNDV0f2RSaiPj7Jma4CPO8wtQSQFRsEn0KGrp/u3Q+bRGx1hryRJR3iUVjYP3zG+vEbmJIFBs96VSsWDACMWy5iAjxO+U9Vq1cxnQOa0sshsNptIJpPC7/eLQqHwUZZls2VMoW20VCq5A4GASCQStb4G1kNCaxddcUkS5XJZxONxoaqqCIfDYmFhwYuA9cs4j7oxDMRDPuQ7MzMjIFqLUWcdslEGIOd8Pi/sdvvFTCbTFwqFSMyDwCOcTFWaJUSmWKRXUZTL6CMVi0Vhlut+Giav36Pqx1KMZ7NZBwVEkNoy0nKh7I5EIiKdTvfAZwK+O9HHbJNUEoo2yH8q6CYxzMwRDAZJzAOmaZaYySa0PaU9YF+zHPqNnokpzk2aFXVrrBW2sNgIBD5TO8+ERLbrfbkvrV9BF3NaQcNNLMUj/KFpSygnaWMNNpXEJiFGl7vIy9WrX2rqk8vlRHt7+w0c93taTJS/K2jMplKpIDloJ46ONt0J/C/VLcFmDIQetpJeRDtD9BONRgXizWIgsxQDoqKrq0soPp+P7oodlYM8AiQmWZvNRoON3Way4d3oM0SzisX+3G8ZdW/b2toKCh1HBK6g8iw43kTWpid/mAaou7S0Je6aBo+8DM5x5m6VfQCntcOlP4ZpcAy8aYHIe3CUM7rheZ/jbH63CZH7/GUUs/oKyoBTLLiU2dETQfnwhH4mf/NdN87Qw0U5bhdngE4tqfKT/w5MWO2v1OANaanJ4h/Z/yf0S4ABAI1oQOxFaPzhAAAAAElFTkSuQmCC'

wallet="YOUR_WALLET_ADDRESS"

json=$(curl -s https://api.nanopool.org/v1/xmr/avghashrate/"$wallet")
jsonWorkers=$(curl -s https://api.nanopool.org/v1/xmr/workers/"$wallet")
jsonBalance=$(curl -s https://api.nanopool.org/v1/xmr/balance/"$wallet")

h1=$(echo "$json" | /usr/local/bin/jq -r '.data.h1')
h3=$(echo "$json" | /usr/local/bin/jq -r '.data.h3')
h6=$(echo "$json" | /usr/local/bin/jq -r '.data.h6')
h12=$(echo "$json" | /usr/local/bin/jq -r '.data.h12')
h24=$(echo "$json" | /usr/local/bin/jq -r '.data.h24')

balance=$(echo "$jsonBalance" | /usr/local/bin/jq -r '.data')


printf "%s H/s | image=%s\n" "$h1" "$moneroIconBase64"
echo "---"
printf "3H: %s H/s | image=%s\n" "$h3" "$moneroIconBase64"
printf "6H: %s H/s | image=%s\n" "$h6" "$moneroIconBase64"
printf "12H: %s H/s | image=%s\n" "$h12" "$moneroIconBase64"
printf "24H: %s H/s | image=%s\n" "$h24" "$moneroIconBase64"
echo "---"
arr=( $(echo "$jsonWorkers" | /usr/local/bin/jq -r '.data[].id') )
arrh=( $(echo "$jsonWorkers" | /usr/local/bin/jq -r '.data[].hashrate') )
arrl=( $(echo "$jsonWorkers" | /usr/local/bin/jq -r '.data[].lastShare') )
# printf 'Worker: %s : %s : %s\n' "${arr[@]} ${arrh[@]} ${arrl[@]}"
i=0
now=$(date +%s)
for item in ${arr[*]}
do
    last=${arrl[$i]}
    let "count = $now - $last"
    printf "%s: (%s H/s) Last: %s secs \n" $item ${arrh[$i]} $count
    i=$[$(echo $i) + 1]
done
echo "---"
printf "Balance: %s XMR\n" "$balance"
echo "---"
echo "Nanopool Dashboard | href=\"https://xmr.nanopool.org/account/"$wallet"\""
