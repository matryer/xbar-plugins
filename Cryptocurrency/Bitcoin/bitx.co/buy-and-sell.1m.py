#!/usr/bin/env python
# <bitbar.title>BitX Buy and Sell</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Konrad Blum</bitbar.author>
# <bitbar.author.github>kblum</bitbar.author.github>
# <bitbar.desc>Shows latest buy and sell values (in ZAR) for Bitcoins on the BitX exchange.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/868608/24828465/e257fc20-1c5d-11e7-9641-5abed32bcb9b.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>


import urllib2
import json
from datetime import datetime


def run():
    base_api_url = 'https://api.mybitx.com'

    currency_pair = 'XBTZAR'
    url = '{0}/api/1/ticker?pair={1}'.format(base_api_url, currency_pair)
    
    response = urllib2.urlopen(url).read()
    data = json.loads(response)

    timestamp = datetime.fromtimestamp(int(data['timestamp'])/1000.0)

    # lines below will cycle through in menu bar
    print('Buy: R{0:,}'.format(float(data['ask'])))
    print('Sell: R{0:,}'.format(float(data['bid'])))

    print('---')

    # lines below will only appear when opening menu
    print('BitX Market Data - XBT/ZAR')
    print('Updated: {0:%Y-%m-%d %H:%M:%S}'.format(timestamp))
    

if __name__ == '__main__':
    run()
