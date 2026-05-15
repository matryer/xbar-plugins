#!/usr/bin/env -S PATH="${PATH}:/opt/homebrew/bin:/usr/local/bin" python3
# Metadata allows your plugin to show up in the app, and website.
#
#  <xbar.title>Nightscout monitor</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Michal Terbert</xbar.author>
#  <xbar.desc>Shows current glucose level, trend, timestamp, and device battery status</xbar.desc>
#  <xbar.dependencies>python3, python requests</xbar.dependencies>
#  <xbar.abouturl>https://diab.ninja/</xbar.abouturl>
#  <xbar.image>https://diab.ninja/xbar.png</xbar.image>
#
#  <xbar.var>string(VAR_NS=""): Your Nightscout instance URL (ex. https://nightscout.diab.ninja).</xbar.var>
#  <xbar.var>string(VAR_API_TOKEN=""): Your Nigthscout API-TOKEN.</xbar.var>

import datetime
import os
import hashlib

######### CONFIGURATION
BASE = os.environ.get("VAR_NS")
API_TOKEN = os.environ.get("VAR_API_TOKEN")
#########

if not BASE:
    raise Exception("Nightscout website not set")

try:
    import requests
except ImportError as e:
    printError("Requests module not found. Install it by running 'pip install requests'.", e)

def get_json(url, params=None):
    if params is None:
        params = {}

    headers = {}
    if API_TOKEN:
        headers["api-secret"] = hashlib.sha1(API_TOKEN.encode()).hexdigest()

    r = requests.get(url, params=params, headers=headers, verify=False, timeout=10)
    r.raise_for_status()
    return r.json()

try:
    entries = get_json(f"{BASE}/api/v1/entries.json", {"count": 1})
    devices = get_json(f"{BASE}/api/v1/devicestatus/", {"count": 1})

    if devices:
        device_data = devices[0]
        device = device_data.get("device", "unknown")
        battery = device_data.get("uploader", {}).get("battery", "?")
    else:
        device = None
        battery = None

    entry = entries[0]
    timestamp = entry["date"] // 1000
    sugar = entry["sgv"]
    direction = entry.get("direction", "")

    direction_map = {
        "DoubleUp": "⬆",
        "SingleUp": "↑",
        "FortyFiveUp": "↗",
        "Flat": "➡",
        "FortyFiveDown": "↘",
        "SingleDown": "↓",
        "DoubleDown": "⬇"
    }
    tr_arr = direction_map.get(direction, "?")


    # sugar status 
    sugar_p = f"{'❤️' if sugar < 75 or sugar > 160 else '💚'} {sugar}"

    # time
    date_time = datetime.datetime.fromtimestamp(timestamp).strftime('%H:%M / %d-%m-%Y')

    # output
    print(f"{sugar_p} {tr_arr}")
    print("---")
    print(f"📉 Trend: {tr_arr} | color=orange")
    print("---")
    print(f"⏰ {date_time} | color=orange")
    print("---")
    if device and battery is not None:
        print(f"🔋 {device}: {battery}% | color=orange")
    else:
        print("🔋 no device data | color=gray")   
    print("---")
    print(f"👉 {BASE} | href={BASE}")
    print("---")
    print(f"🥷 diab.ninja | href=https://diab.ninja | color=white")
    print(f"App version: v1.0 | disabled=true | size=10")

except Exception as e:
    print("no data")
    print("ERROR:", e)
