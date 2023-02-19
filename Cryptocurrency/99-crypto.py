#!/usr/bin/env python3

# <xbar.title>Crypto Ticker ($1USD)</xbar.title>
# <xbar.version>v2.1</xbar.version>
# <xbar.author>davidosomething</xbar.author>
# <xbar.author.github>davidosomething</xbar.author.github>
# <xbar.desc>
#   Displays current crypto/$1 from Coinmarketcap
# </xbar.desc>
# <xbar.image>https://i.imgur.com/B1nq4AU.jpg</xbar.image>

import json
from urllib.request import Request, urlopen

API_KEY = ''

TICKERS = [
    {
        'symbol': 'BTC',
        'sign': 'B',
    },
    {
        'symbol': 'ETH',
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

    headers = {'X-CMC_PRO_API_KEY': API_KEY, 'Accept': 'application/json'}
    symbols = ",".join(list(map(lambda x: x['symbol'], TICKERS)))
    req = Request('https://pro-api.coinmarketcap.com/v2/cryptocurrency/quotes/latest?symbol=%s' % symbols, None, headers)
    data = urlopen(req).read()
    result = json.loads(data.decode('utf-8'))

    for ticker in TICKERS:
        currency = 'usd'
        symbol = ticker['symbol']

        value = float(result['data'][symbol][0]['quote']['USD']['price'])
        is_up = result['data'][symbol][0]['quote']['USD']['volume_change_24h'] > 0

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
        print((output % value))

    print('---')
    print('Refresh | refresh=true')


if __name__ == "__main__":
    main()
