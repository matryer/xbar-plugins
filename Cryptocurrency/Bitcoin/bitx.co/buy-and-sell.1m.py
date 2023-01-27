#!/usr/bin/env python3

# <xbar.title>BitX Buy and Sell</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Konrad Blum</xbar.author>
# <xbar.author.github>kblum</xbar.author.github>
# <xbar.desc>Shows latest buy and sell values (in ZAR) for Bitcoins on the BitX exchange.</xbar.desc>
# <xbar.image>https://cloud.githubusercontent.com/assets/868608/24828465/e257fc20-1c5d-11e7-9641-5abed32bcb9b.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import urllib.request, urllib.error, urllib.parse
import json
from datetime import datetime


def run():
    base_api_url = 'https://api.mybitx.com'

    currency_pair = 'XBTZAR'
    url = '{0}/api/1/ticker?pair={1}'.format(base_api_url, currency_pair)
    
    response = urllib.request.urlopen(url).read()
    data = json.loads(response)

    timestamp = datetime.fromtimestamp(int(data['timestamp'])/1000.0)

    # lines below will cycle through in menu bar
    print(('Buy: R{0:,}'.format(float(data['ask']))))
    print(('Sell: R{0:,}'.format(float(data['bid']))))

    print('---')

    # lines below will only appear when opening menu
    print('BitX Market Data - XBT/ZAR')
    print(('Updated: {0:%Y-%m-%d %H:%M:%S}'.format(timestamp)))
    

if __name__ == '__main__':
    run()
