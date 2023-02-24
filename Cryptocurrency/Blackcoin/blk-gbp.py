#!/usr/bin/env python3

# <xbar.title>Blackcoin Ticker (£1GBP)</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>impshum</xbar.author>
# <xbar.author.github>impshum</xbar.author.github>
# <xbar.desc>Displays current Blackcoin price for £1 from Coinmarketcap</xbar.desc>
# <xbar.image>https://i.imgur.com/guazVdD.jpg</xbar.image>
#
# by impshum

import json
from urllib.request import Request, urlopen

API_KEY = ''

headers = {'X-CMC_PRO_API_KEY': API_KEY, 'Accept': 'application/json'}
req = Request('https://pro-api.coinmarketcap.com/v2/cryptocurrency/quotes/latest?symbol=BLK&convert=GBP', None, headers)
data = urlopen(req).read()
result = json.loads(data.decode('utf-8'))

gbp = result['data']['BLK'][0]['quote']['GBP']

def flow():
    if gbp['volume_change_24h'] > 0:
        print((' £%.4f | image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QAyQACAALwzISXAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACkSBTjB+AAAALNJREFUOMvVk70NAjEMhb87WYiGBZAQU7ABNSVSWpZgEEagsJDoKBELUCEKFuBuCKTw0xyQC0lICe5i+/k9/wT+3opUUJQhcAUqa8I5ZQT4tANwioGTCkQZA9vmOQE2oUJFhL0DXBz33RpKUfCLfLTQJMx9IlEWuQr6QB3prGtNS1lwiMvEYo7ekNsKRBkB+y+rH1hDFVOwy7ids+gbVzrsM6CXeYDTF85xroB1ZoHb73ymB5RhJkpZTihGAAAAAElFTkSuQmCC' % float(gbp['price'])))
    else:
        print((' £%.4f | image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QABACnAADQ9FZaAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACQ1FZwK3gAAAMRJREFUOMvNkjEKAjEQRZ+jKNjYKh5AbzCdjVcQj+BFPIKlp7EMeAJrUbASQVCEr80uG9cNbqe/Cgn/5WUI/DqNfBHM+kCzbs+lPUAr2pwBq5qABbB+M8gszkDvS/kOdAG5VBgEM4ApsP0CGLukjxlEoA0wSZR3Lo0qhxhZDIBDAmDA0wsBLD51CZeOwLKivHbprZx6AkAHuEXbD5fawYwywMqAzOKeDTTPvKqcTGZBMLsGs0utn5gADYEHcKp9e9ni//MCDtNCE3qjsIwAAAAASUVORK5CYII=' % float(gbp['price'])))

flow()
