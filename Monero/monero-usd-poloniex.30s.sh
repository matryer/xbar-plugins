#!/bin/bash

# <bitbar.title>Monero USD price at Poloniex</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Monero.how</bitbar.author>
# <bitbar.author.github>monerohow</bitbar.author.github>
# <bitbar.desc>Shows the last Monero price (in USD) on the Poloniex exchange</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/monerohow/misc/master/mac-menu-bar-light-and-dark.jpg</bitbar.image>
# <bitbar.abouturl>https://www.monero.how</bitbar.abouturl>
#
# Visit https://www.monero.how for Monero tutorials, paper wallets and the latest Monero news
# 

moneroIconBase64='iVBORw0KGgoAAAANSUhEUgAAABoAAAAeCAYAAAAy2w7YAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAE3RFWHRTb2Z0d2FyZQBtb25lcm8uaG9398/iYQAAAyxJREFUeNrclstrE1EUxu9MJkWrtvWxKmgXRrG4KBRELFUqVtFKqnUnLkRF1L9AwYXBpeBCEVcuXIgbQRG0rYraGokP4iok5KHZqJi0TUwnD9I8/U48I9Mwk7FNcOGBX7i599zz3ee5I7lcLmFh/eAgGAAOsA5IIAm+AA+YBN5GQZQGbXvBJTAMZIP2DWArOASuglfgGnhuFMwowApwG7wEB0x86o1muA88A3fAKiuhTvAYXODOy7Ez4AlYbya0EjzgWTRrQ+AhWG0kdB3sF62zPeBWvdBucJ7L0+BrEwI/wGsun9QGrwld0e2JGxwGsWWIJIATvNDV0f2RSaiPj7Jma4CPO8wtQSQFRsEn0KGrp/u3Q+bRGx1hryRJR3iUVjYP3zG+vEbmJIFBs96VSsWDACMWy5iAjxO+U9Vq1cxnQOa0sshsNptIJpPC7/eLQqHwUZZls2VMoW20VCq5A4GASCQStb4G1kNCaxddcUkS5XJZxONxoaqqCIfDYmFhwYuA9cs4j7oxDMRDPuQ7MzMjIFqLUWcdslEGIOd8Pi/sdvvFTCbTFwqFSMyDwCOcTFWaJUSmWKRXUZTL6CMVi0Vhlut+Giav36Pqx1KMZ7NZBwVEkNoy0nKh7I5EIiKdTvfAZwK+O9HHbJNUEoo2yH8q6CYxzMwRDAZJzAOmaZaYySa0PaU9YF+zHPqNnokpzk2aFXVrrBW2sNgIBD5TO8+ERLbrfbkvrV9BF3NaQcNNLMUj/KFpSygnaWMNNpXEJiFGl7vIy9WrX2rqk8vlRHt7+w0c93taTJS/K2jMplKpIDloJ46ONt0J/C/VLcFmDIQetpJeRDtD9BONRgXizWIgsxQDoqKrq0soPp+P7oodlYM8AiQmWZvNRoON3Way4d3oM0SzisX+3G8ZdW/b2toKCh1HBK6g8iw43kTWpid/mAaou7S0Je6aBo+8DM5x5m6VfQCntcOlP4ZpcAy8aYHIe3CUM7rheZ/jbH63CZH7/GUUs/oKyoBTLLiU2dETQfnwhH4mf/NdN87Qw0U5bhdngE4tqfKT/w5MWO2v1OANaanJ4h/Z/yf0S4ABAI1oQOxFaPzhAAAAAElFTkSuQmCC'

xmrLast=$(curl -s https://poloniex.com/public?command=returnTicker | tr '}' '\n' | grep USDT_XMR | tr -d '{}"' | tr ':,' '\n' | grep -A1 "last" | tail -1)
printf "$%.*f | image=%s\n" 2 "$xmrLast" "$moneroIconBase64"
echo "---"
echo "Live chart | href=\"https://www.monero.how/monero-chart\""
echo "Monero.how homepage | href=\"https://www.monero.how\""

