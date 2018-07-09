#!/usr/local/homebrew/bin/python3
"""Loads Ethereum wallet address (configured below) and all tokens associated with
   each address. Then displays current $USD value in the bitbar title, with a drop-down
   showing each total value for owned ETH and each token.
"""
import time
from collections import defaultdict

import requests

#
# Enter your wallets here:
#
WALLET_ADDRESSES = [
    '0x88752Bd600928b902EDAd9afFaaFCE1367Ae3461',
    '0x418091020F2A909479C4058E32BE08464A45DA8A',
]

# Tickers to display at the bottom:
TICKERS = ['BTC', 'ETH', 'LTC']

# <bitbar.title>Etherum Wallet (and token) Balances</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Charlie Schluting</bitbar.author>
# <bitbar.author.github>manos</bitbar.author.github>
# <bitbar.desc>Displays a $USD total across all configured wallets, with drop-down showing the value of your ETH and all tokens held.</bitbar.desc>
# <bitbar.image>https://schluting.com/temp/bitbar-eth-bal.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/manos/bitbar_ethereum_wallet_balances</bitbar.abouturl>

ETHEXPLORER_API_KEY = 'freekey'
EE_URL = 'http://api.ethplorer.io/'
EE_QS = '?apiKey=%s' % ETHEXPLORER_API_KEY
CMC_LISTINGS = []

def get_json(url):
    """Fetches URL and returns JSON. """
    res = requests.get(url)
    res.raise_for_status()
    return res.json()

def get_ticker_price(ticker):
    """Returns current price (from coinmarketcap.com) for given symbol. """
    ticker_id = [x['id'] for x in CMC_LISTINGS['data'] if x['symbol'] == ticker][0]

    resp = get_json('https://api.coinmarketcap.com/v2/ticker/%s/' % ticker_id)
    return resp['data']['quotes']['USD']['price']


if __name__ == '__main__':
    eth_sum = 0
    my_tokens = defaultdict(lambda: 0)

    # Load up coinmarketcap.com listings:
    CMC_LISTINGS = get_json('https://api.coinmarketcap.com/v2/listings/')

    for addr in WALLET_ADDRESSES:
        resp = get_json(EE_URL + 'getAddressInfo/' + addr + EE_QS)

        eth_sum += resp.get('ETH', {}).get('balance', 0)

        if 'tokens' in resp and resp['tokens']:
            for token in resp['tokens']:
                if not token['balance']:
                    continue
                if int(token['tokenInfo']['decimals']):
                    # update the balance with proper decimal shift:
                    token['balance'] = token['balance'] * 10**-int(token['tokenInfo']['decimals'])
                # convert balance to USD now:
                if token['tokenInfo']['price']:
                    token_price = float(token['tokenInfo']['price']['rate'])
                else:
                    token_price = 0
                my_tokens[token['tokenInfo']['symbol']] = defaultdict(float)
                my_tokens[token['tokenInfo']['symbol']]['price'] = token_price
                my_tokens[token['tokenInfo']['symbol']]['count'] += float(token['balance'])
                my_tokens[token['tokenInfo']['symbol']]['usd'] += float(token['balance']) * token_price
        time.sleep(1)

    eth_price = get_ticker_price('ETH')
    my_eth_bal = float(eth_price) * float(eth_sum)

    # total $USD amount:
    total_bal = sum(x['usd'] for x in my_tokens.values()) + my_eth_bal
    print("%.2f" % total_bal)
    print('---')
    print('Balances:')
    print("ETH %.2f|color=green" % my_eth_bal)
    print('-- Tokens: %f|color=black' % eth_sum)
    print('-- Price: %f|color=black' % eth_price)

    for token in sorted((x for x in my_tokens.items()), key=lambda x: x[1]['usd'], reverse=True):
        print(token[0], "%.2f|color=green" % token[1]['usd'])
        print('-- Tokens: %f|color=black' % token[1]['count'])
        print('-- Price: %f|color=black' % token[1]['price'])

    # links to wallet addresses on etherscan.io:
    print('Wallets:')
    for wallet in WALLET_ADDRESSES:
        print("Etherscan: %s" % wallet[:6] + "|href=https://etherscan.io/address/" + wallet + " color=blue")

    print("Prices:")
    for ticker in TICKERS:
        print("%s: %s|color=green" % (ticker, str(get_ticker_price(ticker))))
