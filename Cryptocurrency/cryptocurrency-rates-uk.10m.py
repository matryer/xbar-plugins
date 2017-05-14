#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Cryptocurrency rates UK</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Alvin</bitbar.author>
# <bitbar.author.github>alvinio</bitbar.author.github>
# <bitbar.desc>Displays Cryptocurrency rates for UK</bitbar.desc>
# <bitbar.image>http://i.imgur.com/pYEF7hB.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/alvinio/bitbar-plugins</bitbar.abouturl>
#
# by Alvin
import urllib2
from json import JSONDecoder

try:
    request = urllib2.Request(url="https://cryptomate.co.uk/api/all/GBP/")
    response = urllib2.urlopen(request).read()

    decoded_response = JSONDecoder().decode(response)

    print('1 ETH: £{}'.format(decoded_response['ETH']['price']))
    print('1 BTC: £{}'.format(decoded_response['BTC']['price']))
    print('1 LTC: £{}'.format(decoded_response['LTC']['price']))
    print('1 XRP: £{}'.format(decoded_response['XRP']['price']))
    print("---")
    print('1 DASH: £{}'.format(decoded_response['DASH']['price']))
    print('1 DOGE: £{}'.format(decoded_response['DOGE']['price']))
    print('1 BTS: £{}'.format(decoded_response['BTS']['price']))

except Exception as e:
    print('{}'.format(e))