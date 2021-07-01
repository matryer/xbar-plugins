#!/usr/bin/env python2
# coding: utf-8

# <xbar.title>Ethereum Gas Price</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>yc</xbar.author>
# <xbar.author.github>ichuan</xbar.author.github>
# <xbar.desc>Show current ethereum gas price (fast and standard) in Gwei</xbar.desc>
# <xbar.image>https://emojipedia-us.s3.dualstack.us-west-1.amazonaws.com/thumbs/120/google/263/fuel-pump_26fd.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.gasnow.org/</xbar.abouturl>
# <xbar.var>string(VAR_FONT_NAME=""): Font name.</xbar.var>

import os
import json
import urllib2


GAS_NOW_API = "https://www.gasnow.org/api/v3/gas/price?utm_source=xbar-egp"
USUAL_GAS_RANGE = [90, 150]


def get_gas_now():
    return json.load(urllib2.urlopen(GAS_NOW_API))


def get_fast_avg_gas():
    ret = get_gas_now()
    return {
        "fast": int(ret["data"]["fast"] / 1e9),
        "standard": int(ret["data"]["standard"] / 1e9),
    }


def get_extra_parameters():
    params = []
    font_name = os.environ.get("VAR_FONT_NAME")
    if font_name:
        params.append('font="{}"'.format(font_name.replace('"', '\\"')))
    return " ".join(params)


def main():
    try:
        gas = get_fast_avg_gas()
        bar = "{fast}/{standard}".format(**gas)
        menu = "Fast {fast} Gwei, Standard {standard} Gwei".format(**gas)
        if gas["fast"] < USUAL_GAS_RANGE[0]:
            color = "green"
        elif gas["fast"] > USUAL_GAS_RANGE[1]:
            color = "red"
        else:
            color = "black"
    except Exception:
        bar = "-"
        menu = "Click to reveal"
        color = "red"
    color = "white"
    bar = u"⛽ {} | color={} size=12 {}".format(bar, color, get_extra_parameters())
    print bar.encode("utf-8")
    print "---"
    print 'Ethereum Gas Price: {} | href="{}"'.format(menu, GAS_NOW_API)


if __name__ == "__main__":
    main()
