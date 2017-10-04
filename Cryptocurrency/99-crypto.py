#!/usr/bin/env python
# coding=utf-8
"""
# <bitbar.title>Crypto Ticker ($1USD)</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>davidosomething</bitbar.author>
# <bitbar.author.github>davidosomething</bitbar.author.github>
# <bitbar.desc>
#   Displays current crypto/$1 from Coinmarketcap
# </bitbar.desc>
# <bitbar.image>https://i.imgur.com/B1nq4AU.jpg</bitbar.image>
"""

import json
import urllib2

TICKERS = [
    {
        'symbol': 'btc',
        'sign': 'B',
    },
    {
        'symbol': 'eth',
        'sign': '𝚵',
    },
]

SYMBOLS = {
    'up': 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QAyQACAALwzIS'
          'XAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACkSBTjB+AAAALNJREFUOM'
          'vVk70NAjEMhb87WYiGBZAQU7ABNSVSWpZgEEagsJDoKBELUCEKFuBuCKTw0xyQC0lIC'
          'e5i+/k9/wT+3opUUJQhcAUqa8I5ZQT4tANwioGTCkQZA9vmOQE2oUJFhL0DXBz33RpK'
          'UfCLfLTQJMx9IlEWuQr6QB3prGtNS1lwiMvEYo7ekNsKRBkB+y+rH1hDFVOwy7ids+g'
          'bVzrsM6CXeYDTF85xroB1ZoHb73ymB5RhJkpZTihGAAAAAElFTkSuQmCC',
    'down': 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QABACnAADQ9'
            'FZaAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACQ1FZwK3gAAAMRJRE'
            'FUOMvNkjEKAjEQRZ+jKNjYKh5AbzCdjVcQj+BFPIKlp7EMeAJrUbASQVCEr80uG9c'
            'Nbqe/Cgn/5WUI/DqNfBHM+kCzbs+lPUAr2pwBq5qABbB+M8gszkDvS/kOdAG5VBgE'
            'M4ApsP0CGLukjxlEoA0wSZR3Lo0qhxhZDIBDAmDA0wsBLD51CZeOwLKivHbprZx6A'
            'kAHuEXbD5fawYwywMqAzOKeDTTPvKqcTGZBMLsGs0utn5gADYEHcKp9e9ni//MCDt'
            'NCE3qjsIwAAAAASUVORK5CYII=',
}


def main():
    """
    Display movement icon, symbol, price
    """

    for ticker in TICKERS:
        currency = 'usd'
        symbol = ticker['symbol']

        """cryptomate
        """
        currency = currency.upper()
        symbol = symbol.upper()
        api_base = 'https://cryptomate.co.uk/api/'
        url = api_base + symbol + '/' + currency

        """coinmarketcap
        api_base = 'https://coinmarketcap-nexuist.rhcloud.com/api/'
        url = api_base + symbol
        """

        request = urllib2.Request(url)
        response = urllib2.urlopen(request).read()
        result = json.loads(response)

        """cryptomate
        """
        value = float(result[symbol]['price'])
        is_up = result[symbol]['change'] > 0

        """coinmarketcap
        value = float(result['price']['usd'])
        is_up = result['change'] > 0
        """

        # symbol = SYMBOLS['up' if result['change'] > 0 else 'down']
        if is_up:
            symbol = ':chart_with_upwards_trend: '
        else:
            symbol = ':chart_with_downwards_trend: '
        output = ''.join((
            # ' ',
            symbol,
            ticker['sign'],
            '%.2f',
            ' | size=12'
            # '| image=', symbol,
            # ' color=#000000'
        ))
        print(output % value)

    print('---')
    print('Refresh | refresh=true')


if __name__ == "__main__":
    main()
