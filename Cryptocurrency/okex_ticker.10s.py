#!/usr/bin/env python2
# coding: utf-8
# yc@2021/06/30

# <xbar.title>OKEx Ticker</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>yc</xbar.author>
# <xbar.author.github>ichuan</xbar.author.github>
# <xbar.desc>Show price ticker from OKEx. Currencies can be configured.</xbar.desc>
# <xbar.image>https://i.imgur.com/mykIv6O.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.okex.com/about.html</xbar.abouturl>
# <xbar.var>string(VAR_CURRENCIES="btc,eth"): Currencies, separated with comma.</xbar.var>
# <xbar.var>string(VAR_FONT_NAME=""): Font name.</xbar.var>

import re
import os
import json
import urllib2
from datetime import datetime
from decimal import Decimal


HTTP_TIMEOUT = 10
DEFAULT_CURRENCIES = "btc,eth"


def normalize_decimal(val, precision=2):
    if type(val) is not Decimal:
        val = Decimal(val)
    mul = pow(10, precision)
    val = Decimal(int(val * mul)) / mul
    return format(val.normalize(), "f")


def get_update_time(timestamp):
    updated_at = datetime.strptime(timestamp, "%Y-%m-%dT%H:%M:%S.%fZ")
    delta = (datetime.utcnow() - updated_at).total_seconds()
    ret = []
    h, s = divmod(int(delta), 3600)
    if h:
        ret.append("{} hours".format(h))
    m, s = divmod(s, 60)
    if m:
        ret.append("{} minutes".format(m))
    if s:
        ret.append("{} seconds".format(s))
    if ret:
        return "{} ago".format(" ".join(ret))
    return "just now"


def http_get_json(url):
    req = urllib2.Request(
        url,
        headers={
            "User-Agent": "curl/7.55",
            "Cache-Control": "max-age=0",
            "accept": "*/*",
        },
    )
    return json.load(urllib2.urlopen(req, timeout=HTTP_TIMEOUT))


def get_ticker_info(currency):
    url = "https://www.okex.com/api/spot/v3/instruments/{}-USDT/ticker".format(currency)
    try:
        resp = http_get_json(url)
        updated_at = get_update_time(resp["timestamp"])
        change_24h = (
            (float(resp["last"]) - float(resp["open_24h"]))
            / float(resp["open_24h"])
            * 100
        )
        return {
            "currency": currency,
            "title": "${} ({}%)".format(
                normalize_decimal(resp["last"]), normalize_decimal(change_24h),
            ),
            "updated_at": updated_at,
            "24h_high_low": "{} / {}".format(resp["high_24h"], resp["low_24h"]),
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
    print("Open www.okex.com | href=https://www.okex.com/")


if __name__ == "__main__":
    main()
