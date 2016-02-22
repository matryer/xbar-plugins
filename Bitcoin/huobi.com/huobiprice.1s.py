#!/usr/bin/python
# coding=utf-8

# <bitbar.title>Huobi last price</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sam Xie</bitbar.author>
# <bitbar.author.github>mountain3th</bitbar.author.github>
# <bitbar.desc>A very simple huobi last price display tool</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by mountain3th/Sam Xie

import urllib2


def price():
    response = urllib2.urlopen('https://api.huobi.com/staticmarket/td_btc.html').read()
    lines = response.split('\n')
    last_line = lines[-2]
    last_second_line = lines[-3]
    open_price = float(lines[2])
    last_price = float(last_second_line.split(',')[1])
    current_price = float(last_line.split(',')[1])
    return open_price, last_price, current_price

def prices_output():
    open_price, last_price, current_price = price()
    return u'火币网:￥%s%s  %.2f' % (current_price, u'⬆️' if current_price > last_price else u'⬇️', (current_price - open_price) / open_price * 100) + '%'

if __name__ == '__main__':
    print prices_output().encode('utf-8')
