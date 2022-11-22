#!/usr/bin/env /usr/local/bin/python3
# coding=utf-8

"""
# <xbar.title>Cardano (ADA) Price Monitor</xbar.title>
# <xbar.version>v0.1</xbar.version>
# <xbar.author>Erol Soyöz</xbar.author>
# <xbar.author.github>soyoz</xbar.author.github>
# <xbar.desc>You can monitor Cardano (ADA)'s latest price via CoinRanking.com API</xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>http://soyoz.com/</xbar.abouturl>
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
        coinId = '9'
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
