#!/usr/bin/env python3
# coding: utf-8
# yc@2021/06/30

# <xbar.title>OKX Ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>yc</xbar.author>
# <xbar.author.github>ichuan</xbar.author.github>
# <xbar.desc>Show price ticker from OKX. Currencies can be configured.</xbar.desc>
# <xbar.image>https://i.imgur.com/mykIv6O.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.okx.com/about.html</xbar.abouturl>
# <xbar.var>string(VAR_CURRENCIES="btc,eth"): Currencies, separated with comma.</xbar.var>
# <xbar.var>string(VAR_FONT_NAME=""): Font name.</xbar.var>

import re
import os
import json
from datetime import datetime
from decimal import Decimal
from urllib.request import urlopen, Request


HTTP_TIMEOUT = 10
DEFAULT_CURRENCIES = "btc,eth"


def normalize_decimal(val, precision=2):
    if type(val) is not Decimal:
        val = Decimal(val)
    mul = pow(10, precision)
    val = Decimal(int(val * mul)) / mul
    return format(val.normalize(), "f")


def get_update_time(timestamp):
    updated_at = datetime.utcfromtimestamp(int(timestamp) // 1000)
    delta = (datetime.utcnow() - updated_at).total_seconds()
    if delta < 1:
        return "just now"
    ret = []
    h, s = divmod(int(delta), 3600)
    if h:
        ret.append("{} hours".format(h))
    m, s = divmod(s, 60)
    if m:
        ret.append("{} minutes".format(m))
    if s:
        ret.append("{} seconds".format(s))
    return "{} ago".format(" ".join(ret))


def http_get_json(url):
    req = Request(
        url,
        headers={
            "User-Agent": "curl/7.55",
            "Cache-Control": "max-age=0",
            "accept": "*/*",
        },
    )
    return json.load(urlopen(req, timeout=HTTP_TIMEOUT))


def get_ticker_info(currency):
    url = "https://www.okx.com/api/v5/market/ticker?instId={}-USDT".format(currency)
    try:
        resp = http_get_json(url)["data"][0]
        updated_at = get_update_time(resp["ts"])
        change_24h = (
            (float(resp["last"]) - float(resp["open24h"]))
            / float(resp["open24h"])
            * 100
        )
        return {
            "currency": currency,
            "title": "${} ({}%)".format(
                normalize_decimal(resp["last"]),
                normalize_decimal(change_24h),
            ),
            "updated_at": updated_at,
            "24h_high_low": "{} / {}".format(resp["high24h"], resp["low24h"]),
        }
    except Exception as e:
        return {
            "currency": currency,
            "title": "ERR",
            "error": str(e),
        }


def get_extra_parameters():
    params = []
    font_name = os.environ.get("VAR_FONT_NAME")
    if font_name:
        params.append('font="{}"'.format(font_name.replace('"', '\\"')))
    return " ".join(params)


def main():
    currencies = os.environ.get("VAR_CURRENCIES", DEFAULT_CURRENCIES)
    currencies = [i.upper() for i in re.split(r"\s*,\s*", currencies) if i]
    infos = [get_ticker_info(i) for i in currencies]
    titles = ["{currency}: {title}".format(**i) for i in infos]
    title = "{} | color=white size=12 {}".format(
        "    ".join(titles), get_extra_parameters()
    )
    last_updates = [
        "--{}: {}".format(i["currency"], i["updated_at"])
        for i in infos
        if "updated_at" in i
    ]
    high_lows = [
        "--{}: {}".format(i["currency"], i["24h_high_low"])
        for i in infos
        if "24h_high_low" in i
    ]
    print(title)
    print("---")
    if last_updates:
        print("Last update")
        for i in last_updates:
            print(i)
    if high_lows:
        print("24 hours high/low")
        for i in high_lows:
            print(i)
    for i in infos:
        if "error" in i:
            print("{}: {} | color=red".format(i["currency"], i["error"]))
    print("Open www.okx.com | href=https://www.okx.com/")


if __name__ == "__main__":
    main()
