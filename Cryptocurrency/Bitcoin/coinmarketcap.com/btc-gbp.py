#!/usr/bin/python
# coding=utf-8
#
# <xbar.title>Bitcoin coin Ticker (£1GBP)</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>impshum</xbar.author>
# <xbar.author.github>impshum</xbar.author.github>
# <xbar.desc>Displays current Bitcoin price for £1 from Coinmarketcap</xbar.desc>
# <xbar.image>iVBORw0KGgoAAAANSUhEUgAAAKEAAABRAQMAAACADVTsAAAABlBMVEUiIiL///9ehyAxAAABrElEQVR4Xu3QL2/bQBgG8NdRlrnMNqxu1eVAahCQVAEF03STbsuBSFVZYEBBoJ2RjZ0Hljuy6IZaUlUlpfsKRUmZP4JTNJixkEm7nJu/Mxlot0l7JJOfXj06P/D3xvkBQH/lqoEC7WVvzqM0k/f4+Gat2nt7ppqeCjCbiJX6HmN7vnca4LLc0BljH/yZ0ZejDQXGlA9GmYSthoumVw1wZ6PByxjrpxmeZq0hbMcDXPCHGVB4hHCAkgUKrrNSulawelPRCH37mu4fR1EdZYPwnTA6UZoQfteoMSmPCFVcgYmUmmCuPMKkIAtNFjqS+hWyOo+MzmVsb12NS1aFazThe1Ztr2qYBklWvcPKCKG+TA/MGwjqDcI4n1Pko+1E5KM9TRz75fGB0qWv1Vlq/Bo9Gzqo3oqu7g991G1bVQmp8IQcdeRtEGpyxoVVB5eNLob0qS6xpaJc5+J7Wx+wkwct5SoSn2vCOORKrHZk0lC69tAbm4a2g0grEuknvd9tb61XhqK8hz+d/xG/cft5fD0dvxA7qsLrj+EXWqBugRbeHl6qcbCr4Ba+7Tn88/kJk4CIztd1IrIAAAAASUVORK5CYII=</xbar.image>
#
# by impshum

from urllib import urlopen
url = urlopen('https://coinmarketcap-nexuist.rhcloud.com/api/btc').read()

import json
result = json.loads(url)

def flow():
    if result ['change'] > '0':
        print (' £%.4f | image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QAyQACAALwzISXAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACkSBTjB+AAAALNJREFUOMvVk70NAjEMhb87WYiGBZAQU7ABNSVSWpZgEEagsJDoKBELUCEKFuBuCKTw0xyQC0lICe5i+/k9/wT+3opUUJQhcAUqa8I5ZQT4tANwioGTCkQZA9vmOQE2oUJFhL0DXBz33RpKUfCLfLTQJMx9IlEWuQr6QB3prGtNS1lwiMvEYo7ekNsKRBkB+y+rH1hDFVOwy7ids+gbVzrsM6CXeYDTF85xroB1ZoHb73ymB5RhJkpZTihGAAAAAElFTkSuQmCC color=#000000'% float(result['price']['gbp']))
    else:
        print (' £%.4f | image=iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABmJLR0QABACnAADQ9FZaAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4AQHACQ1FZwK3gAAAMRJREFUOMvNkjEKAjEQRZ+jKNjYKh5AbzCdjVcQj+BFPIKlp7EMeAJrUbASQVCEr80uG9cNbqe/Cgn/5WUI/DqNfBHM+kCzbs+lPUAr2pwBq5qABbB+M8gszkDvS/kOdAG5VBgEM4ApsP0CGLukjxlEoA0wSZR3Lo0qhxhZDIBDAmDA0wsBLD51CZeOwLKivHbprZx6AkAHuEXbD5fawYwywMqAzOKeDTTPvKqcTGZBMLsGs0utn5gADYEHcKp9e9ni//MCDtNCE3qjsIwAAAAASUVORK5CYII= color=#000000'% float(result['price']['gbp']))

flow()
