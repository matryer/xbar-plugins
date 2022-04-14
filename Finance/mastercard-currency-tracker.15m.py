#!/usr/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Currency Tracker Mastercard</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Alp Sayin</xbar.author>
# <xbar.author.github>alpsayin</xbar.author.github>
# <xbar.desc>Keep an eye on Mastercard currency exchange rates</xbar.desc>
# <xbar.dependencies>python3, requests</xbar.dependencies>
# <xbar.abouturl>https://alpsayin.com/#xbar-plugins</xbar.abouturl>
# <xbar.image>https://github.com/alpsayin/alpsayin.github.com/raw/master/img/bitbar_mastercard.png</xbar.image> 

import requests
import json
import traceback
from pprint import pprint

# Only 1 currency pair is supported for now
# Duplicate the script if more pairs are needed
currency_from = 'USD'
currency_to = 'TRY'

# curl 'https://www.mastercard.us/settlement/currencyrate/conversion-rate?fxDate=0000-00-00&transCurr=USD&crdhldBillCurr=GBP&bankFee=0&transAmt=1' 
# -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0' 
# -H 'Accept: application/json, text/plain, */*' 
# -H 'Accept-Language: en-GB,en;q=0.5' 
# -H 'Accept-Encoding: gzip, deflate, br' 
# -H 'DNT: 1' 
# -H 'Connection: keep-alive' 
# -H 'Referer: https://www.mastercard.us/en-us/personal/get-support/convert-currency.html' 
# -H 'Sec-Fetch-Dest: empty' 
# -H 'Sec-Fetch-Mode: cors' 
# -H 'Sec-Fetch-Site: same-origin' 
# -H 'Sec-GPC: 1' 
# -H 'TE: trailers'

headers = {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0',
      'Accept': 'application/json, text/plain, */*',
      'Accept-Language': 'en-GB,en;q=0.5',
      'Accept-Encoding': 'gzip, deflate, br',
      'DNT': '1',
      'Connection': 'keep-alive',
      'Referer': 'https://www.mastercard.us/en-us/personal/get-support/convert-currency.html',
      'Sec-Fetch-Dest': 'empty',
      'Sec-Fetch-Mode': 'cors',
      'Sec-Fetch-Site': 'same-origin',
      'Sec-GPC': '1',
      'TE': 'trailers',
}

url = f'https://www.mastercard.us/settlement/currencyrate/conversion-rate?fxDate=0000-00-00&transCurr={currency_from}&crdhldBillCurr={currency_to}&bankFee=0&transAmt=1'

def main():
      result = requests.get(url, headers=headers)
      if result.status_code != 200:
            print(f'HTTP {result.status_code}')
            return

      try:
            result_dict = json.loads(result.text)
      except Exception as exc:
            print('JSON syntax error')
            print('---')
            pprint(result.text)
            print(f'{exc}')
            traceback.print_exc()
            return

      try:
            result = result_dict['data']['conversionRate']
      except Exception as exc:
            print('JSON key error')
            print('---')
            pprint(result_dict)
            print(f'{exc}')
            traceback.print_exc()
            return

      print(f'{currency_from}:{currency_to} {result:.2f}')
      print('---')
      print(f'{currency_to}:{currency_from} {1.0/result:.3f}')

main()
