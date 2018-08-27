#!/usr/bin/env /usr/local/bin/python3
# coding=utf-8

"""
# <bitbar.title>Cardano (ADA) Price Monitor</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Erol Soyöz</bitbar.author>
# <bitbar.author.github>soyoz</bitbar.author.github>
# <bitbar.desc>You can monitor Cardano (ADA)'s latest price via CoinRanking.com API</bitbar.desc>
# <bitbar.image>https://s15.postimg.cc/4yheazrln/ada.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>
# <bitbar.abouturl>http://soyoz.com/</bitbar.abouturl>
"""

import json
import textwrap
from urllib.request import urlopen

""
""

COLORS = {
    'red': '#b50000',
    'green': '#03a600'
}

"""
"""


class BitBarAPI:
    @staticmethod
    def seperate():
        seperate = '---'
        return seperate

    @staticmethod
    def color(hex):
        color = 'color=' + hex
        return color

    @staticmethod
    def font(font):
        font = 'font=' + font
        return font

    @staticmethod
    def refresh(status):
        refresh = 'refresh=' + status
        return refresh


"""
"""


class CoinRankingAPI:
    @staticmethod
    def getCoin(base, coinId):
        try:
            connect = urlopen(
                'https://api.coinranking.com/v1/public/coin/' + coinId + '?base=' + base).read()
        except:
            return False

        return connect


"""
"""


class Cardano:
    @staticmethod
    def main():
        currency = 'USD'
        coinId = '2207'
        getCoin = CoinRankingAPI.getCoin(currency, coinId)
        if (getCoin):
            result = json.loads(getCoin)

            baseSign = result['data']['base']['sign']
            symbol = result['data']['coin']['symbol']
            price = float(result['data']['coin']['price'])
            description = result['data']['coin']['description']
            percentChange24h = result['data']['coin']['change']

            if (percentChange24h >= 0):
                color = COLORS['green']
            else:
                color = COLORS['red']

            outputPrice = '{} ' + baseSign + '{:.3f} | ' + \
                BitBarAPI.font('HelveticaNeue-Light') + \
                ' ' + BitBarAPI.color(color)
            outputPrice = outputPrice.format(
                symbol,
                price
            )
            print(outputPrice)
            print(BitBarAPI.seperate())

            outputPercentChange = '24H CHANGE: {}%'
            outputPercentChange = outputPercentChange.format(
                percentChange24h,
            )
            print(outputPercentChange)
            print(BitBarAPI.seperate())

            print(textwrap.fill(description, 30))
        else:
            print('ERROR! | ' + BitBarAPI.color(COLORS['red']))
            print(BitBarAPI.seperate())
            print('Failed to connect! | ' + BitBarAPI.color(COLORS['red']))

        print(BitBarAPI.seperate())
        print('Refresh | ' + BitBarAPI.refresh('true'))


"""
Run!
"""
Cardano.main()
