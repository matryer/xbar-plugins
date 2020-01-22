#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python

'''
    All metadata is placed here
'''
# <bitbar.title>Nomics.com Cryptocurrency Tickers</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Daniel Anderson</bitbar.author>
# <bitbar.author.github>dtand</bitbar.author.github>
# <bitbar.desc>Provides price updates and 24h change for the top ten cryptocurrencies by marketcap.</bitbar.desc>
# <bitbar.image>https://i.ibb.co/4SD8cZs/Screen-Shot-2019-11-25-at-6-16-56-PM.png</bitbar.image>
# <bitbar.dependencies>python,requests</bitbar.dependencies>
# <bitbar.abouturl>https://nomics.com</bitbar.abouturl>
import requests


## Base url for api
BASE_URL = 'https://api.nomics.com/v1'

## Tickers endpoint
ENDPOINT = 'currencies/ticker'

## FREE API key
API_KEY = '4465bf5e9801e08b9a3e04084c7ea3c3'

## Returns top ten cryptocurrencies
def get_top_ten():
    url = "{}/currencies/ticker?key={}".format(BASE_URL, API_KEY)
    response = requests.get(url)
    data = response.json()
    top_ten = []
    i = 0 
    for ticker in data:
        if i >= 10:
            break
        top_ten.append(ticker["id"])
        i = i + 1
    return top_ten

## Returns ticker for provided symbol
def get_tickers(symbols):
    params = {
        "ids": symbols
    }
    url = '{}/{}?key={}'.format(BASE_URL, ENDPOINT, API_KEY)
    response = requests.get(url, params=params)
    data = response.json()
    return data

## Pads string to certain length
def pad_string(string, length):
    padding = length - len(string)
    return string.ljust(padding)

## Returns the string specification for stdout
## for bitbar interpreter
def generate_bitbar_format(tickers):
    std_out_strings = []
    i = 1
    for ticker in tickers:
        symbol   = ticker['symbol']

        price    = round(float(ticker['price']),4)

        if price < 1.00:
            price = '${:,.4f}'.format(price)
        else:
            price = '${:,.2f}'.format(price)

        p = abs(float(ticker['1d']['price_change_pct']))
        percent  = round(p * 100, 2)
        percent  = str(percent) + "%"

        col_one = pad_string(symbol + ": " + price, 24)

        if float(ticker['1d']['price_change_pct']) >= 0:
            std_out_strings.append(col_one + "\t+" + percent + " | color=green")
        else:
            std_out_strings.append(col_one + "\t-" + percent + " | color=red")

        i = i + 1

    return std_out_strings

## Print output with bitbar formatting between new lines
def output_values(values):
    print(" Nomics.com | image=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")
    print("Go to nomics.com for all assets | href=https://nomics.com")
    for i,value in enumerate(values):
        print(value)
    print('Refresh | refresh=true')

## Wrapper method for generating output and printing it
def std_out(tickers):
    output_values(generate_bitbar_format(tickers))

def main():
    top_ten = get_top_ten()
    tickers = get_tickers(",".join(top_ten))
    std_out(tickers)

if __name__ == "__main__":
    main()

