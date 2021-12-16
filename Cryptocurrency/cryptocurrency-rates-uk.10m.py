#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <xbar.title>Cryptocurrency rates UK</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Alvin</xbar.author>
# <xbar.author.github>alvinio</xbar.author.github>
# <xbar.desc>Displays Cryptocurrency rates for UK</xbar.desc>
# <xbar.image>http://i.imgur.com/pYEF7hB.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/alvinio/bitbar-plugins</xbar.abouturl>
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