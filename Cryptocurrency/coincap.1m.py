#!/usr/bin/env python3

# <xbar.title>Crypto tickers (CoinPaprika)</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Peter Stenger, Mateusz Sroka</xbar.author>
# <xbar.author.github>reteps,donbagger</xbar.author.github>
# <xbar.desc>Retrieves trading information about coins from the free CoinPaprika API, no key needed. Change is over the last 24h, open/high/low are for the current UTC day.</xbar.desc>
# <xbar.image>https://i.imgur.com/a584lGl.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://coinpaprika.com</xbar.abouturl>

# CoinPaprika coin ids, full list at https://api.coinpaprika.com/v1/coins
coins_usd = ['btc-bitcoin', 'eth-ethereum', 'ltc-litecoin', 'miota-iota'] #USD

coins_btc = ['neo-neo', 'xlm-stellar', 'xmr-monero', 'xno-nano'] #BTC
# raiblocks is called nano these days; waltonchain no longer trades anywhere,
# so it was dropped from the defaults

#------------------------------BEGIN CODE------------------------------#
import json
from urllib.request import Request, urlopen

API = 'https://api.coinpaprika.com/v1'

# the API rejects the default Python-urllib user agent, hence the header
def fetch(path):
    request = Request(API + path, headers={'User-Agent': 'xbar-plugin'})
    with urlopen(request, timeout=15) as response:
        return json.load(response)

print('Ƀ')
print('---')
standard = "|href='https://coinpaprika.com/coin/{}/' font='Menlo'"
usd = "{: <5} {:0<9.3f} {:0<+6.2f}% {:0<9.3f} {:0<9.3f} {:0<9.3f}  {:0>3}" + standard
btc = "{: <5} {:0<9.7f} {:0<+6.2f}% {:0<9.7f} {:0<9.7f} {:0<9.7f}  {:0>3}" + standard

#----DISPLAY----#
def print_rows(coins, quote, fmt):
    for coin in coins:
        ticker = fetch('/tickers/{}?quotes={}'.format(coin, quote))
        ohlcv = fetch('/coins/{}/ohlcv/today?quote={}'.format(coin, quote.lower()))[0]
        data = ticker['quotes'][quote]
        print(fmt.format(ticker['symbol'], data['price'], data['percent_change_24h'],
                         ohlcv['open'], ohlcv['high'], ohlcv['low'], ticker['rank'], coin))

print('COIN     USD     CHANGE   OPEN      HIGH       LOW    RANK|font="Menlo"')
print_rows(coins_usd, 'USD', usd)
print('COIN     BTC     CHANGE   OPEN      HIGH       LOW    RANK|font="Menlo"')
print_rows(coins_btc, 'BTC', btc)
