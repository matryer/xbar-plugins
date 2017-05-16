#!/usr/bin/env python

# <bitbar.title>Coinone Ticker</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sunwoo Lee</bitbar.author>
# <bitbar.author.github>eldkqmfhf123</bitbar.author.github>
# <bitbar.desc>coineone.co.kr Ticker</bitbar.desc>


import urllib2
import json


def parse():
    base_url = 'https://api.coinone.co.kr/ticker/?currency='
    # You can change it to btc / eth / etc / xrp
    currency = 'btc'

    url = base_url + currency

    response = urllib2.urlopen(url).read()
    response_data = json.loads(response)

    print "{0}: {1}".format(currency.upper(), response_data['last'])
    print "---"
    print "By Skriex | href=http://github.com/eldkqmfhf123"


if __name__ == '__main__':
    parse()
