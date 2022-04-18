#!/usr/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>Currency Tracker Duckduckgo</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Alp Sayin</xbar.author>
# <xbar.author.github>alpsayin</xbar.author.github>
# <xbar.desc>Keep an eye on currency exchange rates from DuckDuckGo</xbar.desc>
# <xbar.dependencies>python3, requests</xbar.dependencies>
# <xbar.abouturl>https://alpsayin.com/#xbar-plugins</xbar.abouturl>
# <xbar.image>https://github.com/alpsayin/alpsayin.github.com/raw/master/img/xbar_currency_tracker_duckduckgo.png</xbar.image> 

import requests
import json
import traceback
from pprint import pprint

# Only 1 currency pair is supported for now
# Duplicate the script if more pairs are needed
currency_from = 'USD'
currency_to = 'TRY'

headers = {
      'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:99.0) Gecko/20100101 Firefox/99.0',
      'Accept': 'text/plain, */*; q=0.01',
      'Accept-Language': 'en-GB,en;q=0.5',
      'Accept-Encoding': 'text',
      'Referer': 'https://duckduckgo.com/',
      'X-Requested-With': 'XMLHttpRequest',
      'DNT': '1',
      'Connection': 'keep-alive',
      'Sec-Fetch-Dest': 'empty',
      'Sec-Fetch-Mode': 'cors',
      'Sec-Fetch-Site': 'same-origin',
      'Sec-GPC': '1',
}

url=f'https://duckduckgo.com/js/spice/currency/1/{currency_from}/{currency_to}'

def main():
      result = requests.get(url, headers=headers)
      if result.status_code != 200:
            print(f'HTTP {result.status_code}')
            return

      stripped = result.text.strip('ddg_spice_currency(').strip(');\n')

      try:
            result_dict = json.loads(stripped)
      except Exception as exc:
            print('JSON syntax error')
            print('---')
            pprint(result.text)
            print(f'{exc}')
            traceback.print_exc()
            return

      # pprint(result_dict)

      try:
            result = float(result_dict['conversion']['converted-amount'])
            timestamp = result_dict['conversion']['rate-utc-timestamp']
      except Exception as exc:
            print('JSON key error')
            print('---')
            pprint(result_dict)
            print(f'{exc}')
            traceback.print_exc()
            return

      print(f'{currency_from}:{currency_to} {result:.3f}')
      print('---')
      print(f'{currency_to}:{currency_from} {1.0/result:.3f}')
      print(f'{timestamp}')

main()
