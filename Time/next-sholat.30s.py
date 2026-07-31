#!/usr/bin/env python3
#
# <xbar.title>Next Sholat</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Muhammad Fatkurozi</xbar.author>
# <xbar.author.github>ibnumardini</xbar.author.github>
# <xbar.desc>Countdown to the next prayer time in Indonesia (Aladhan API, Kemenag RI method)</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

import json
import math
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request
from datetime import datetime, timedelta
from pathlib import Path

METHOD = 20 # Kemenag RI method
CACHE_DIR = Path.home() / ".cache" / "nextsholat"
LOCATION_FILE = CACHE_DIR / "location.txt"
GEOCODE_CACHE_FILE = CACHE_DIR / "geocode.json"
NOTIFIED_FILE = CACHE_DIR / "notified.txt"
NOTIFIED_ARRIVED_FILE = CACHE_DIR / "notified-arrived.txt"
REMINDER_MINUTES = 10
JUST_PASSED_WINDOW_S = 10 * 60
HTTP_TIMEOUT = 5

# Locations are hardcoded for speed, but you can also search for a city/subdistrict name
LOC_ORDER = [
    "jakarta",
    "bandung",
    "surabaya",
    "medan",
    "makassar",
    "semarang",
    "yogyakarta",
    "bandar_lampung",
]
DEFAULT_LOC = "jakarta"

LOCATIONS = {
    "jakarta": {"lat": -6.2088, "lon": 106.8456, "label": "Jakarta"},
    "bandung": {"lat": -6.9175, "lon": 107.6191, "label": "Bandung"},
    "surabaya": {"lat": -7.2575, "lon": 112.7521, "label": "Surabaya"},
    "medan": {"lat": 3.5952, "lon": 98.6722, "label": "Medan"},
    "makassar": {"lat": -5.1477, "lon": 119.4327, "label": "Makassar"},
    "semarang": {"lat": -6.9932, "lon": 110.4203, "label": "Semarang"},
    "yogyakarta": {"lat": -7.7956, "lon": 110.3695, "label": "Yogyakarta"},
    "bandar_lampung": {"lat": -5.4292, "lon": 105.2610, "label": "Bandar Lampung"},
}

NAMES = ["Fajr", "Dhuhr", "Asr", "Maghrib", "Isha"]


def http_get_json(url, headers=None):
    req = urllib.request.Request(url, headers=headers or {})
    with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as res:
        return json.loads(res.read().decode("utf-8"))


def read_geocode_cache():
    try:
        return json.loads(GEOCODE_CACHE_FILE.read_text())
    except (OSError, ValueError):
        return {}


def write_geocode_cache(cache):
    GEOCODE_CACHE_FILE.write_text(json.dumps(cache))


def geocode_city(city_name):
    cache = read_geocode_cache()
    if city_name in cache:
        return {"ok": True, **cache[city_name]}

    query = urllib.parse.quote(f"{city_name}, Indonesia")
    url = f"https://nominatim.openstreetmap.org/search?q={query}&format=json&limit=1"
    try:
        results = http_get_json(url, headers={"User-Agent": "nextsholat-xbar-plugin"})
        if not results:
            return {"ok": False, "reason": f'City not found: "{city_name}"'}
        lat = float(results[0]["lat"])
        lon = float(results[0]["lon"])
        label = ",".join(results[0]["display_name"].split(",")[:2]).strip()
        cache[city_name] = {"lat": lat, "lon": lon, "label": label}
        write_geocode_cache(cache)
        return {"ok": True, "lat": lat, "lon": lon, "label": label}
    except Exception as err:
        return {"ok": False, "reason": f"Geocoding failed: {err}"}


# key is either a LOCATIONS preset id, or a raw city name typed via Search city...
def resolve_location(key):
    if key in LOCATIONS:
        return {"ok": True, **LOCATIONS[key]}
    return geocode_city(key)


def display_name(name, date_string):
    if name == "Dhuhr":
        if datetime.strptime(date_string, "%Y-%m-%d").weekday() == 4:  # Friday
            return "Jumu'ah"
    return name


def date_str(d):
    return d.strftime("%Y-%m-%d")


def clean_old_cache():
    cutoff = datetime.now().timestamp() - 2 * 24 * 60 * 60
    for f in CACHE_DIR.iterdir():
        if f.stat().st_mtime < cutoff:
            f.unlink()


def fetch_day(date_string, out_file, loc):
    y, m, d = date_string.split("-")
    url = (
        f"https://api.aladhan.com/v1/timings/{d}-{m}-{y}"
        f"?latitude={loc['lat']}&longitude={loc['lon']}&method={METHOD}"
    )
    try:
        data = http_get_json(url)
        if data.get("code") != 200:
            reason = data["data"] if isinstance(data.get("data"), str) else "Failed to fetch"
            return {"ok": False, "reason": reason}
        out_file.write_text(json.dumps(data))
        return {"ok": True, "data": data}
    except Exception as err:
        return {"ok": False, "reason": f"Network error: {err}"}


def read_cache(file):
    try:
        return json.loads(file.read_text())
    except (OSError, ValueError):
        return None


def timing_hhmm(data, name):
    return data["data"]["timings"][name].split(" ")[0]


def ts_from_local(date_string, hhmm):
    return datetime.strptime(f"{date_string} {hhmm}", "%Y-%m-%d %H:%M").timestamp()


def send_notification(title, message):
    message = message.replace('"', '\\"')
    title = title.replace('"', '\\"')
    script = f'display notification "{message}" with title "{title}" sound name "Glass"'
    try:
        subprocess.run(["osascript", "-e", script], check=True, capture_output=True, timeout=HTTP_TIMEOUT)
    except Exception:
        pass  # ignore notification failures


def read_marker(file):
    try:
        return file.read_text().strip()
    except OSError:
        return ""


def notify_once(marker_file, name, ts, message):
    notify_key = f"{date_str(datetime.fromtimestamp(ts))}-{name}"
    if read_marker(marker_file) == notify_key:
        return
    send_notification("Next Sholat", message)
    marker_file.write_text(notify_key)


def maybe_notify_reminder(next_name, next_ts, now_ts):
    minutes_left = (next_ts - now_ts) / 60
    if minutes_left > REMINDER_MINUTES or minutes_left <= 0:
        return
    notify_once(NOTIFIED_FILE, next_name, next_ts, f"{next_name} in {math.ceil(minutes_left)} minutes")


def maybe_notify_arrived(name, ts, now_ts):
    if now_ts < ts or now_ts - ts >= JUST_PASSED_WINDOW_S:
        return
    notify_once(NOTIFIED_ARRIVED_FILE, name, ts, f"It's time for {name} \U0001F33C")


def print_menu(next_line):
    print("--:--")
    print("---")
    print(next_line)


def print_location_error(message, script_path):
    print_menu(message)
    print(f"Search city... | bash='{script_path}' param1=--set-city-prompt terminal=false refresh=true")
    print("Refresh | refresh=true")


def print_prayer_times(data, today_str, highlight_name, city_label):
    print(f"Today's Prayer Times - {city_label}")
    for name in NAMES:
        t = timing_hhmm(data, name)
        label = display_name(name, today_str)
        if name == highlight_name:
            print(f"**{label}: {t}**")
        else:
            print(f"{label}: {t}")


def print_location_menu(loc_key, city_label, script_path):
    print("Location")
    for key in LOC_ORDER:
        label = LOCATIONS[key]["label"]
        if key == loc_key:
            print(f"--**{label}**")
        else:
            print(f"--{label} | bash='{script_path}' param1=--set-location={key} terminal=false refresh=true")
    if loc_key not in LOCATIONS:
        print(f"--**{city_label} (custom)**")
    print(f"--Search city... | bash='{script_path}' param1=--set-city-prompt terminal=false refresh=true")
    print("---")
    print("Refresh | refresh=true")


def main():
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    clean_old_cache()

    script_path = os.path.abspath(sys.argv[0])
    arg = sys.argv[1] if len(sys.argv) > 1 else ""

    if arg.startswith("--set-location=") or arg.startswith("--set-city="):
        LOCATION_FILE.write_text(arg.split("=", 1)[1])
        return
    if arg == "--set-city-prompt":
        script = 'text returned of (display dialog "City / subdistrict name:" default answer "" with title "Next Sholat")'
        try:
            result = subprocess.run(["osascript", "-e", script], check=True, capture_output=True, text=True)
            city = result.stdout.strip()
            if city:
                LOCATION_FILE.write_text(city)
        except Exception:
            pass  # user cancelled
        return

    loc_key = read_marker(LOCATION_FILE) or DEFAULT_LOC

    loc = resolve_location(loc_key)
    if not loc["ok"]:
        print_location_error(f"Location error - {loc['reason']}", script_path)
        return
    city_label = loc["label"]
    cache_key = re.sub(r"[^a-zA-Z0-9_-]", "_", loc_key)

    now = datetime.now()
    today_str = date_str(now)
    tomorrow_str = date_str(now + timedelta(days=1))

    cache_today = CACHE_DIR / f"{today_str}-{cache_key}.json"
    cache_tomorrow = CACHE_DIR / f"{tomorrow_str}-{cache_key}.json"

    data_today = read_cache(cache_today)
    if not data_today:
        result = fetch_day(today_str, cache_today, loc)
        if not result["ok"]:
            print_location_error(f"Failed to fetch prayer times - {result['reason']}", script_path)
            return
        data_today = result["data"]

    now_ts = now.timestamp()
    next_name, next_ts = "", 0
    last_name, last_ts = "", 0

    for name in NAMES:
        hhmm = timing_hhmm(data_today, name)
        ts = ts_from_local(today_str, hhmm)
        if ts > now_ts and (not next_name or ts < next_ts):
            next_name, next_ts = name, ts
        if ts <= now_ts and ts > last_ts:
            last_name, last_ts = name, ts

    last_label = display_name(last_name, today_str) if last_name else ""

    if next_name:
        maybe_notify_reminder(next_name, next_ts, now_ts)
    if last_name:
        maybe_notify_arrived(last_label, last_ts, now_ts)

    def print_footer(highlight_name):
        print("---")
        print_prayer_times(data_today, today_str, highlight_name, city_label)
        print("---")
        print_location_menu(loc_key, city_label, script_path)

    if last_name and now_ts - last_ts < JUST_PASSED_WINDOW_S:
        ago_min = int((now_ts - last_ts) // 60)
        if ago_min == 0:
            print(f"It's time for {last_label} \U0001F33C")
        else:
            print(f"{last_label} was {ago_min}m ago")
        print_footer(last_name)
        return

    # All prayers today passed -> next is Fajr tomorrow
    if not next_name:
        data_tomorrow = read_cache(cache_tomorrow)
        if not data_tomorrow:
            result = fetch_day(tomorrow_str, cache_tomorrow, loc)
            if result["ok"]:
                data_tomorrow = result["data"]
        if data_tomorrow:
            hhmm = timing_hhmm(data_tomorrow, "Fajr")
            next_ts = ts_from_local(tomorrow_str, hhmm)
            next_name = "Fajr"
            maybe_notify_reminder(next_name, next_ts, now_ts)

    if not next_ts:
        print_menu("Failed to compute schedule. Refresh | refresh=true")
        return

    diff = int(next_ts - now_ts)
    hh = diff // 3600
    mm = (diff % 3600) // 60
    next_label = display_name(next_name, today_str)

    if hh == 0 and mm == 0:
        print(f"It's time for {next_label} \U0001F33C")
    else:
        countdown = f"{mm}m" if hh == 0 else f"{hh}h {mm}m"
        print(f"{next_label} in {countdown}")
    print_footer(next_name)


if __name__ == "__main__":
    main()
