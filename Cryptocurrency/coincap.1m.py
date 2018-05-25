#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3

# <bitbar.title>CoinCap</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Peter Stenger</bitbar.author>
# <bitbar.author.github>reteps</bitbar.author.github>
# <bitbar.desc>Retrieves trading information about a coin on cryptocompare and coinmarketcap. High & low not available on CMC.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/a584lGl.png</bitbar.image>
# <bitbar.dependencies>python3,requests</bitbar.dependencies>

coins_usd = ['bitcoin','ethereum','litecoin'] #USD

coins_btc = ['neo','walton','stellar','monero'] #BTC

coins_cmcbtc = ['raiblocks'] #Coinmarketcap BTC

coins_cmcusd = ['iota'] #CoinmarketCap USD

#------------------------------BEGIN CODE------------------------------#
import requests
print('Ƀ')
print('---')
coin_data_usd = {}
coin_data_btc = {}
standard = "|href='https://coinmarketcap.com/currencies/{}' font='Menlo'"
usd = "{: <5} {:0<9.3f} {:0<+6.2f}% {:0<9.3f} {:0<9.3f} {:0<9.3f}  {:0>3}" + standard
btc = "{: <5} {:0<9.7f} {:0<+6.2f}% {:0<9.7f} {:0<9.7f} {:0<9.7f}  {:0>3}" + standard
#----DATA----#
for coin in coins_usd:
    data = requests.get("https://api.coinmarketcap.com/v1/ticker/{}".format(coin)).json()[0]
    coin_data_usd[data["symbol"]] = data['rank']
for coin in coins_btc:
    data = requests.get("https://api.coinmarketcap.com/v1/ticker/{}".format(coin)).json()[0]
    coin_data_btc[data["symbol"]] = data['rank'] 
raw_usd = requests.get('https://min-api.cryptocompare.com/data/pricemultifull?fsyms={}&tsyms=USD'.format(','.join(coin_data_usd.keys()))).json()['RAW']
raw_btc = requests.get('https://min-api.cryptocompare.com/data/pricemultifull?fsyms={}&tsyms=BTC'.format(','.join(coin_data_btc.keys()))).json()['RAW']
raw_cmcbtc = [requests.get('https://api.coinmarketcap.com/v1/ticker/{}'.format(coin)).json()[0] for coin in coins_cmcbtc]
raw_cmcusd = [requests.get('https://api.coinmarketcap.com/v1/ticker/{}'.format(coin)).json()[0] for coin in coins_cmcusd]
#---HELPER---#
def f(x):
    return float(x)

#----DISPLAY----#
print('COIN     USD     CHANGE   OPEN      HIGH       LOW    RANK|font="Menlo"')
#---USD---#
for i, coin in enumerate(coin_data_usd.keys()):
    data = raw_usd[coin]["USD"]
    print(usd.format(coin,data["PRICE"],data['CHANGEPCT24HOUR'],data['OPEN24HOUR'],data['HIGH24HOUR'],data['LOW24HOUR'],coin_data_usd[coin],coins_usd[i]))
for i, coin in enumerate(coins_cmcusd):
    data = raw_cmcusd[i]
    print(usd.format(data["symbol"],f(data["price_usd"]),f(data['percent_change_24h']),f(data["price_usd"])*((100-f(data['percent_change_24h']))/100),0.000,0.000,data['rank'],coin))
#---BTC---#
print('COIN     BTC     CHANGE   OPEN      HIGH       LOW    RANK|font="Menlo"')
for i, coin in enumerate(coin_data_btc.keys()):
    data = raw_btc[coin]["BTC"]
    print(btc.format(coin,data["PRICE"],data['CHANGEPCT24HOUR'],data['OPEN24HOUR'],data['HIGH24HOUR'],data['LOW24HOUR'],coin_data_btc[coin],coins_btc[i]))
for i, coin in enumerate(coins_cmcbtc):
    data = raw_cmcbtc[i]
    print(btc.format(data["symbol"],f(data["price_btc"]),f(data['percent_change_24h']),f(data["price_btc"])*((100-f(data['percent_change_24h']))/100),0.000,0.000,data['rank'],coin))
