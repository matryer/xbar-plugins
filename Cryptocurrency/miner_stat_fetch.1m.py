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
#  <xbar.var>select(VAR_COINTAG="eth"): What coin do you mine? [eth, rvn, xmr]</xbar.var>
#  <xbar.var>select(VAR_VALUTASYMBOL="EUR"): What is your preferred valuta? [EUR, USD, GBP]</xbar.var>
#  <xbar.var>string(VAR_WALLET=""): Your wallet adress.</xbar.var>
#  <xbar.var>number(VAR_HARDWAREHASHRATE=""): Your expected MH/s, this sets the color (if enabled).</xbar.var>
#  <xbar.var>boolean(VAR_SHOWCURRENT=true): Show your current hashrate</xbar.var>
#  <xbar.var>boolean(VAR_SHOWAVG=true): Show your average hashrate</xbar.var>
#  <xbar.var>boolean(VAR_SHOWHOURLYINCOME=true): Show your hourly income bar</xbar.var>
#  <xbar.var>boolean(VAR_SHOWCOLORS=true): Show colors in the top bar</xbar.var>
#  <xbar.var>boolean(VAR_SOLOOPERARION=false): Do you mine solo?</xbar.var>

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
    wallet = json_object['VAR_WALLET'].strip()
    show_current = json_object['VAR_SHOWCURRENT']
    show_avg = json_object['VAR_SHOWAVG']
    mining_pool = json_object['VAR_POOLADRESS'].strip()
    coin_tag = json_object["VAR_COINTAG"]
    valuta_symbol = json_object["VAR_VALUTASYMBOL"]
    hardware_hashrate = json_object["VAR_HARDWAREHASHRATE"]
    show_colors = json_object["VAR_SHOWCOLORS"]
    show_hourly_income = json_object["VAR_SHOWHOURLYINCOME"]
    solo_operation = json_object["VAR_SOLOOPERARION"]
except KeyError:
    print("Failed to load settings, set them in xbar")

top_screen_content = []
sub_menu_content = []

formated_hashrate = ''
formated_current_hashrate = ''
formated_income_last_60_eur = ''

submenu_income_last_60_eur = ''
submenu_current_hashrate = ''
submenu_hashrate = ''

def format_hashrate(prefix: str, hashrate: int):
    # decide the right unit to indicate the fload with (like MH/s or KH/s etc.)
    factor = 1000
    counter = 0
    format_hash_dict = {1000 : " H/s ", 1000_000 : " KH/s ", 1000_000_000 : " MH/s ", 1000_000_000_000 : " GH/s ", 1000_000_000_000_000 : " TH/s "}
    while hashrate / factor > 1:
        factor *= 1000

    recalculated_hashrate = hashrate / factor * 1000
    
    # decide how many decimals should be used
    if recalculated_hashrate / 100 > 1:   
        recalculated_hashrate = "%.0f" % recalculated_hashrate
    elif recalculated_hashrate / 10 > 1:
        recalculated_hashrate = "%.1f" % recalculated_hashrate
    elif recalculated_hashrate / 1 > 1:
        recalculated_hashrate = "%.2f" % recalculated_hashrate

    # throw them together to return a string
    formated_hashrate = prefix + str(recalculated_hashrate) + format_hash_dict[factor]
    return formated_hashrate

# create the api call like : https://eth.2miners.com/api/acconts/MYWALLETADDRES
solo_url_add = ''
if solo_operation is True:
    solo_url_add = "solo-"
url = "https://" + solo_url_add + coin_tag + '.' + mining_pool + "/api/accounts/" + wallet
browsable_url = "https://" + coin_tag + '.' + mining_pool + "/account/" + wallet


try:
    resp = requests.get(url=url)
    pool_data = resp.json()
except JSONDecodeError:
    print("Failed to find wallet address")

# get api info from coinbase about latest currency value
coinbase_api_call = "https://api.coinbase.com/v2/prices/" + coin_tag + '-' + valuta_symbol + "/spot"
coinbase_resp = requests.get(coinbase_api_call)
if coinbase_resp.status_code == 200:
    found_coinbase_data = True
    coinbase_data = coinbase_resp.json()["data"]
    coinbase_value = float(coinbase_data["amount"])
else:
    found_coinbase_data = False


# Format the value CurrentHashrate
hashrate = int(pool_data['currentHashrate'])
formated_hashrate = format_hashrate("Current: ", hashrate)
if show_current is True:
    top_screen_content.append(formated_hashrate)
else:
    sub_menu_content.append(formated_hashrate)


# Format the value AverageHashrate
hashrate = int(pool_data['hashrate'])
formated_hashrate = format_hashrate(" AVG: ", hashrate)
if show_avg is True:
    top_screen_content.append(formated_hashrate)
else:
    sub_menu_content.append(formated_hashrate)

# Income calculation
if found_coinbase_data is True:
    income = list(pool_data['sumrewards'])
    income_last_60 = dict(income[0])
    income_last_60_eth = income_last_60['reward']
    income_last_60_eur = income_last_60_eth * coinbase_value / 1000_000_000
    formated_income_last_60_eur = "Last Hour: " + str("%.2f" % income_last_60_eur) + ' ' + valuta_symbol
else:
    formated_income_last_60_eur ="no coinbase data because: " + str(coinbase_resp)

if show_hourly_income is True:
    top_screen_content.append(formated_income_last_60_eur)
else:
    sub_menu_content.append(formated_income_last_60_eur)

# Output the content in the top screen
final_output = ''
if show_colors is True:
    if hashrate < hardware_hashrate:
        for item in top_screen_content:
            final_output = final_output + item
        print(final_output + " | color=red")
    elif hashrate > hardware_hashrate:
        for item in top_screen_content:
            final_output = final_output + item
        print(final_output + " | color=green")
else:
    for item in top_screen_content:
        final_output = final_output + item
    print(final_output)
    
print("---")
# Output the content in the sub menu
for item in sub_menu_content:
    print(item)

if found_coinbase_data is True:
    print(coin_tag.upper() + " value: " + str(coinbase_value) + ' ' + valuta_symbol)
else:
    print("no coinbase data because: " + str(coinbase_resp))

#Doesn't work yet, no reall apparent reason
##print("Open " + mining_pool + "| href=python=browsable_url")
print("---")
print("Refresh |refresh=true")
print("Support my work | href=https://paypal.me/jorisduyse")