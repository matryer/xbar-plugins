#!/usr/bin/env python3

'''
    All metadata is placed here
'''
# <xbar.title>CoinPaprika Cryptocurrency Tickers</xbar.title>
# <xbar.version>v1.1</xbar.version>
# <xbar.author>Daniel Anderson, Mateusz Sroka</xbar.author>
# <xbar.author.github>dtand,donbagger</xbar.author.github>
# <xbar.desc>Provides price updates and 24h change for the top ten cryptocurrencies by marketcap. Data comes from the free CoinPaprika API, no key needed. This plugin used Nomics before that service shut down.</xbar.desc>
# <xbar.image>https://i.ibb.co/4SD8cZs/Screen-Shot-2019-11-25-at-6-16-56-PM.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://coinpaprika.com</xbar.abouturl>
import json
from urllib.request import Request, urlopen


## Tickers endpoint for the top ten coins by marketcap, no API key required
TICKERS_URL = 'https://api.coinpaprika.com/v1/tickers?quotes=USD&limit=10'

## Returns tickers for the top ten cryptocurrencies
## (the API rejects the default Python-urllib user agent)
def get_tickers():
    request = Request(TICKERS_URL, headers={'User-Agent': 'xbar-plugin'})
    with urlopen(request, timeout=15) as response:
        return json.load(response)

## Pads string to certain length
def pad_string(string, length):
    padding = length - len(string)
    return string.ljust(padding)

## Returns the string specification for stdout
## for the xbar interpreter
def generate_xbar_format(tickers):
    std_out_strings = []
    for ticker in tickers:
        symbol   = ticker['symbol']
        quote    = ticker['quotes']['USD']

        price    = round(float(quote['price']),4)

        if price < 1.00:
            price = '${:,.4f}'.format(price)
        else:
            price = '${:,.2f}'.format(price)

        change   = float(quote['percent_change_24h'])
        percent  = round(abs(change), 2)
        percent  = str(percent) + "%"

        col_one = pad_string(symbol + ": " + price, 24)

        if change >= 0:
            std_out_strings.append(col_one + "\t+" + percent + " | color=green")
        else:
            std_out_strings.append(col_one + "\t-" + percent + " | color=red")

    return std_out_strings

## Print output with xbar formatting between new lines
def output_values(values):
    print("CoinPaprika")
    print("Go to coinpaprika.com for all assets | href=https://coinpaprika.com")
    for value in values:
        print(value)
    print('Refresh | refresh=true')

## Wrapper method for generating output and printing it
def std_out(tickers):
    output_values(generate_xbar_format(tickers))

def main():
    std_out(get_tickers())

if __name__ == "__main__":
    main()
