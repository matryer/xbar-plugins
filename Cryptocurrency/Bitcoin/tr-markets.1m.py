#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3
# <xbar.title>BTC Ticker for TR Markets</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Erhan BÜTE</xbar.author>
# <xbar.author.github>erhan</xbar.author.github>
# <xbar.desc>TC Ticker for TR Markets(btcturk.com, koinim.com, paribu.com)</xbar.desc>
# <xbar.image>https://i.imgur.com/MUj8Bkb.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>

import json
import http.client


def get_btcturk_price():
    try:
        conn = http.client.HTTPSConnection("www.btcturk.com")
        conn.request("GET", "/api/ticker")
        res = conn.getresponse()
        data = json.loads(res.read().decode("utf-8"))
        for item in data:
            if item["pair"] == "BTCTRY":
                return item["ask"]
    except:
        return 0
    return 0


def get_koinim_price():
    try:
        conn = http.client.HTTPSConnection("koinim.com")
        conn.request("GET", "/ticker/")
        res = conn.getresponse()
        data = json.loads(res.read().decode("utf-8"))
        return data["ask"]
    except:
        return 0

def get_paribu_price():
    try:
        conn = http.client.HTTPSConnection("www.paribu.com")
        conn.request("GET", "/ticker")
        res = conn.getresponse()
        data = json.loads(res.read().decode("utf-8"))
        return data["BTC_TL"]["last"]
    except:
        return 0


print("BTCTURK : " + "{0:.2f}".format(get_btcturk_price()) +" TRY")
print("---")
print("KOINIM : " + "{0:.2f}".format(get_koinim_price()) +" TRY")
print("PARIBU : " + "{0:.2f}".format(get_paribu_price()) +" TRY")
