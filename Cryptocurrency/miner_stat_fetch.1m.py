#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/bin/python3

# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Miner Stat Fetch</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Joris Van Duyse</xbar.author>
#  <xbar.author.github>jorisvanduyse</xbar.author.github>
#  <xbar.desc>Chech your miner staticstics! And configer to your wishes</xbar.desc>
#  <xbar.image>https://imgur.com/a/OINHi0W</xbar.image>
#  <xbar.dependencies>python</xbar.dependencies>
#  <xbar.abouturl>https://github.com/jorisvanduyse/MinerFetch_xbar</xbar.abouturl>

# Variables become preferences in the app:
#
#  <xbar.var>string(VAR_POOLADRESS=""): Your wallets pool adress.</xbar.var>
#  <xbar.var>select(VAR_COINTAG="eth"): Which style to use. [eth, rvn, xmr]</xbar.var>
#  <xbar.var>select(VAR_VALUTASYMBOL="EUR"): Which style to use. [EUR, USD, GBP]</xbar.var>
#  <xbar.var>string(VAR_WALLET=""): Your wallet adress.</xbar.var>
#  <xbar.var>number(VAR_HARDWAREHASHRATE=""): Your expected hashrate, this sets the color (if enabled).</xbar.var>
#  <xbar.var>boolean(VAR_SHOWCURRENT=true): Show your current hashrate</xbar.var>
#  <xbar.var>boolean(VAR_SHOWAVG=true): Show your average hashrate</xbar.var>
#  <xbar.var>boolean(VAR_SHOWHOURLYINCOME=true): Show your hourly income bar</xbar.var>
#  <xbar.var>boolean(VAR_SHOWCOLORS=true): Show colors in the top bar</xbar.var>

# All necessary python packages
import json
from json.decoder import JSONDecodeError
import requests
import sys

# Read all the settings given by the user in the xbar program
json_file_name = str(sys.argv[0]) + ".vars.json"
with open(json_file_name) as json_file:
    json_object = json.load(json_file)
    json_file.close()
try:
    wallet = json_object['VAR_WALLET']
    show_current = json_object['VAR_SHOWCURRENT']
    show_avg = json_object['VAR_SHOWAVG']
    mining_pool = json_object['VAR_POOLADRESS']
    coin_tag = json_object["VAR_COINTAG"]
    valuta_symbol = json_object["VAR_VALUTASYMBOL"]
    hardware_hashrate = json_object["VAR_HARDWAREHASHRATE"]
    show_colors = json_object["VAR_SHOWCOLORS"]
    show_hourly_income = json_object["VAR_SHOWHOURLYINCOME"]

except KeyError:
    print("Failed to load settings, set them in xbar")

# create the api call like : https://eth.2miners.com/api/acconts/MYWALLETADDRES
url = "https://" + coin_tag + '.' + mining_pool + "/api/accounts/" + wallet
browsable_url = "https://" + coin_tag + '.' + mining_pool + "/account/" + wallet


try:
    resp = requests.get(url=url)
    data = resp.json()
except JSONDecodeError:
    print("Failed to find wallet address")

coinbase_api_call = "https://api.coinbase.com/v2/prices/" + coin_tag + '-' + valuta_symbol + "/spot"
coinbase_resp = requests.get(coinbase_api_call)
coinbase_data = coinbase_resp.json()["data"]
coinbase_value = float(coinbase_data["amount"])

# Format the value CurrentHashrate
if show_current is True:
    try:
        currentHashrate = int(data['currentHashrate']) / 1_000_000 
        formated_currentHashrate =  "%.0f" % currentHashrate
        formated_currentHashrate = "Current: " + str(formated_currentHashrate) + " MH/s "
    except NameError:
        currentHashrate = "Unknown"
else:
    formated_currentHashrate = ''


# Format the value AverageHashrate
if show_avg is True:
    try:
        hashrate = int(data['hashrate']) / 1_000_000
        formated_hashrate = "%.0f" % hashrate
        formated_hashrate = " AVG: " + str(formated_hashrate) + " MH/s "
    except NameError:
        hashrate = "Unknown"
else:
    formated_hashrate = ''

# Income calculation
if show_hourly_income is True:
    income = list(data['sumrewards'])
    income_last_60 = dict(income[0])
    income_last_60_eth = income_last_60['reward']
    income_last_60_eur = income_last_60_eth * coinbase_value / 1000_000_000
    formated_income_last_60_eur = "Last Hour: " + str("%.2f" % income_last_60_eur) + ' ' + valuta_symbol

else:
    formated_income_last_60_eur = ''

# This line is printed in the top bar
if show_colors is True:
    if hashrate < hardware_hashrate:
        print(formated_currentHashrate + formated_hashrate + formated_income_last_60_eur + "| color=red")
    elif hashrate > hardware_hashrate:
        print(formated_currentHashrate + formated_hashrate + formated_income_last_60_eur +  "| color=green")
else:
    print(formated_currentHashrate + formated_hashrate + formated_income_last_60_eur) 

print("---")

print("---")
#Doesn't work yet, no reall apparent reason
##print("Open " + mining_pool + "| href=python=browsable_url")
print("Refresh |refresh=true")
print("---")
print("Support my work | href=https://paypal.me/jorisduyse")
