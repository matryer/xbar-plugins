#!/usr/bin/env python3

# <xbar.title>Coinone Ticker</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Sunwoo Lee</xbar.author>
# <xbar.author.github>eldkqmfhf123</xbar.author.github>
# <xbar.desc>coineone.co.kr Ticker</xbar.desc>

import urllib.request, urllib.error, urllib.parse
import json

def parse():
    base_url = 'https://api.coinone.co.kr/ticker/?currency='
    # You can change it to btc / eth / etc / xrp
    currency = 'btc'

    url = base_url + currency

    response = urllib.request.urlopen(url).read()
    response_data = json.loads(response)

    print("{0}: {1}".format(currency.upper(), response_data['last']))
    print("---")
    print("By Skriex | href=http://github.com/eldkqmfhf123")


if __name__ == '__main__':
    parse()
