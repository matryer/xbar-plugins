#!/usr/local/bin/python3

#  <xbar.title>CoinMarketCap</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Alvaro Serrano</xbar.author>
#  <xbar.author.github>https://github.com/alvaroserrrano</xbar.author.github>
#  <xbar.desc>Your preferred cryptos in the Fiat currency you choose</xbar.desc>
#  <xbar.image>http://www.hosted-somewhere/pluginimage</xbar.image>
#  <xbar.dependencies>python3</xbar.dependencies>
#  <xbar.abouturl></xbar.abouturl>

# Variables become preferences in the app:
#  <xbar.var>string(VAR_CONVERSION_CURRENCY="USD"): Fiat currency conversion in ISO 8601. See standard for reference (https://www.iso.org/iso-8601-date-and-time-format.html)</xbar.var>
#  <xbar.var>string(VAR_API_KEY=""): Coinmarketcap API KEY. Get yours at https://pro.coinmarketcap.com/</xbar.var>
#  <xbar.var>select(VAR_NUMBER_OF_COINS=""): How many coins to display. [1,5,10]</xbar.var>
#  <xbar.var>select(VAR_SORT_CRITERIA=""): Criteria to select coins by. [market_cap, percent_change_24h, percent_change_7d, market_cap_strict, price, market_cap_by_total_supply_strict, volume_7d, volume_30d, circulating_supply]</xbar.var>
#  <xbar.var>select(VAR_CRYPTO_TYPE=""): Coins or tokens. [all, coins, tokens]</xbar.var>
#  <xbar.var>select(VAR_TIME_SCALE=""): Preferred time range. [1h, 24h, 7d, 30d]</xbar.var>


import os

from requests import Request, Session
from requests.exceptions import ConnectionError, Timeout, TooManyRedirects

BASE_URL = "https://pro-api.coinmarketcap.com"
CMC_VERSION = "v1"
TOP_COIN = os.getenv("VAR_TOP_COIN", "bitcoin")
CONVERSION_CURRENCY = os.getenv("VAR_CONVERSION_CURRENCY", "USD")
NUMBER_OF_COINS = os.getenv("VAR_NUMBER_OF_COINS", "5")
API_KEY = os.getenv("VAR_API_KEY", "")
SORT_CRITERIA = os.getenv("SORT_CRITERIA", "market_cap")
CRYPTO_TYPE = os.getenv("CRYPTO_TYPE", "all")
TIME_SCALE = os.getenv("TIME_SCALE", "24h")
ICONS = {
    "BTC": "₿",
    "USDT": "₮",
    "ETH": "⟠",
    "XMR": "ɱ",
    "XTZ": "ꜩ",
    "ETC": "ξ",
    "DAI": "◈",
    "ZEC": "ⓩ",
    "DOGE": "Ð",
    "XRP": "✕",
    "ADA": "₳",
    "BCH": "Ƀ",
    "BAT": "⟁",
    "THETA": "ϑ",
    "LTC": "Ł",
    "EOS": "ε",
    "BSV": "Ɓ",
}


def show_coins():
    parameters = {
        "start": "1",
        "limit": NUMBER_OF_COINS,
        "convert": CONVERSION_CURRENCY,
        "sort": SORT_CRITERIA,
        "cryptocurrency_type": CRYPTO_TYPE,
    }
    headers = {
        "Accepts": "application/json",
        "X-CMC_PRO_API_KEY": API_KEY,
    }

    session = Session()
    session.headers.update(headers)
    try:
        coins = []
        data = session.get(
            f"{BASE_URL}/{CMC_VERSION}/cryptocurrency/listings/latest",
            params=parameters,
        ).json()["data"]
        for coin in data:
            symbol = coin["symbol"]
            price = round(coin["quote"][CONVERSION_CURRENCY]["price"], 2)
            # circulating_supply = coin["circulating_supply"]
            # total_supply = coin["total_supply"]
            # max_supply = coin["max_supply"]
            if TIME_SCALE == "1h":
                percent_change = round(
                    coin["quote"][CONVERSION_CURRENCY]["percent_change_1h"], 2
                )
            elif TIME_SCALE == "24h":
                percent_change = round(
                    coin["quote"][CONVERSION_CURRENCY]["percent_change_24h"], 2
                )
            elif TIME_SCALE == "7d":
                percent_change = round(
                    coin["quote"][CONVERSION_CURRENCY]["percent_change_7d"], 2
                )
            else:
                percent_change = round(
                    coin["quote"][CONVERSION_CURRENCY]["percent_change_30d"], 2
                )
            coin_display = {
                "symbol": ICONS[symbol] if symbol in ICONS.keys() else symbol,
                "price": price,
                "percent_change": percent_change,
            }
            coins.append(coin_display)
        return coins
    except (ConnectionError, Timeout, TooManyRedirects) as e:
        print(e)


if __name__ == "__main__":
    coins = show_coins()
    symbol = coins[0]["symbol"]
    price = str(coins[0]["price"])
    percentage_change = (
        str(coins[0]["percent_change"]) + "%"
        if coins[0]["percent_change"] < 0
        else "+" + str(coins[0]["percent_change"]) + "%"
    )
    print(
        f"{symbol}: {price} {CONVERSION_CURRENCY} {percentage_change} ({TIME_SCALE})"
    )
    print("---")
    for coin in coins:
        _symbol = coin["symbol"]
        _percentage_change = (
            str(coin["percent_change"]) + "%"
            if coin["percent_change"] < 0
            else "+" + str(coin["percent_change"]) + "%"
        )
        _price = str(coin["price"])
        print(
            f"  {_symbol}      { _price} {CONVERSION_CURRENCY}  {_percentage_change}  ({TIME_SCALE})"
        )
