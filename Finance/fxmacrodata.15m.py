#!/usr/bin/env python3

# <xbar.title>FXMacroData Release Countdown</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Robert Tidball</xbar.author>
# <xbar.author.github>roberttidball</xbar.author.github>
# <xbar.desc>Countdown to the next scheduled macro release (CPI, payrolls, Fed decisions, GDP...) in your menu bar, with the upcoming release calendar and the latest inflation, policy rate and unemployment prints in the dropdown. USD works without an API key.</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://fxmacrodata.com/?utm_source=github&utm_medium=referral&utm_campaign=xbar-plugins&utm_content=readme</xbar.abouturl>

# <xbar.var>string(VAR_FXMACRODATA_API_KEY=""): Optional API key. Not needed for USD; required for other currencies and removes the 15-minute free-tier delay.</xbar.var>
# <xbar.var>select(VAR_CURRENCY="USD"): Currency to follow. Anything other than USD needs an API key. [USD, EUR, GBP, JPY, AUD, CAD, CHF, NZD, NOK, SEK, DKK, CNY, CNH, KRW, TWD, THB, MYR, BRL, PEN, HUF, ILS, NGN]</xbar.var>
# <xbar.var>select(VAR_MIN_IMPORTANCE="medium"): Lowest importance shown in the menu bar countdown. [low, medium, high]</xbar.var>
# <xbar.var>number(VAR_UPCOMING_COUNT=10): How many upcoming releases to list in the dropdown.</xbar.var>

import json
import os
import re
import time
import urllib.error
import urllib.request
from datetime import datetime

API_BASE = "https://api.fxmacrodata.com/v1"
DOCS_URL = "https://fxmacrodata.com/docs"
SITE_URL = "https://fxmacrodata.com"
HTTP_TIMEOUT = 10

LATEST_INDICATORS = [
    ("inflation", "Inflation"),
    ("policy_rate", "Policy rate"),
    ("unemployment", "Unemployment"),
]

IMPORTANCE_RANK = {"low": 0, "medium": 1, "high": 2}
IMPORTANCE_COLOR = {"high": "#d9534f", "medium": "#e8a33d"}

SHORT_NAMES = {
    "inflation": "CPI",
    "core_inflation": "Core CPI",
    "non_farm_payrolls": "NFP",
    "policy_rate": "Rate decision",
    "pce": "PCE",
    "core_pce": "Core PCE",
    "ppi": "PPI",
    "gdp": "GDP",
    "unemployment": "Unemployment",
    "retail_sales": "Retail sales",
    "job_openings": "JOLTS",
    "trade_balance": "Trade balance",
    "initial_jobless_claims": "Jobless claims",
}


def env(name, default=""):
    value = os.environ.get(name, "")
    return value.strip() if value and value.strip() else default


API_KEY = env("VAR_FXMACRODATA_API_KEY")
CURRENCY = env("VAR_CURRENCY", "USD").lower()
MIN_IMPORTANCE = env("VAR_MIN_IMPORTANCE", "medium").lower()
try:
    UPCOMING_COUNT = max(1, int(float(env("VAR_UPCOMING_COUNT", "10"))))
except ValueError:
    UPCOMING_COUNT = 10


class ApiError(Exception):
    pass


def get_json(path):
    headers = {
        "Accept": "application/json",
        "User-Agent": "xbar-fxmacrodata/1.0",
    }
    req = urllib.request.Request(API_BASE + path, headers=headers)
    if API_KEY:
        # The key is only ever sent as a header, never in the URL, and is
        # unredirected so urllib never copies it onto a redirect target.
        req.add_unredirected_header("X-API-Key", API_KEY)
    try:
        with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT) as res:
            payload = json.loads(res.read().decode("utf-8"))
    except urllib.error.HTTPError as exc:
        detail = ""
        try:
            body = json.loads(exc.read().decode("utf-8"))
            detail = body.get("detail") or body.get("error") or ""
        except Exception:
            pass
        raise ApiError("HTTP %s%s" % (exc.code, (": " + str(detail)) if detail else ""))
    except urllib.error.URLError as exc:
        raise ApiError("network error: %s" % exc.reason)
    except (ValueError, OSError) as exc:
        message = str(exc)
        raise ApiError(message.replace(API_KEY, "***") if API_KEY else message)
    if not isinstance(payload, dict) or not isinstance(payload.get("data"), list):
        detail = payload.get("detail") if isinstance(payload, dict) else None
        raise ApiError(str(detail) if detail else "unexpected response")
    return payload


def clean(text):
    # "|" separates xbar parameters, so keep it out of display text.
    return str(text).replace("|", "/").replace("\n", " ").strip()


def short_name(row):
    release = row.get("release", "")
    if release in SHORT_NAMES:
        return SHORT_NAMES[release]
    name = re.sub(r"\s*\(.*?\)", "", row.get("name") or release.replace("_", " "))
    return name.strip() or release


def countdown(seconds):
    seconds = max(0, int(seconds))
    days, rem = divmod(seconds, 86400)
    hours, rem = divmod(rem, 3600)
    minutes = rem // 60
    if days:
        return "%dd %dh" % (days, hours)
    if hours:
        return "%dh %dm" % (hours, minutes)
    return "%dm" % minutes


def local_time(epoch):
    return datetime.fromtimestamp(epoch).strftime("%a %d %b %H:%M")


def upcoming_releases(now):
    payload = get_json("/calendar/%s" % CURRENCY)
    rows = []
    for row in payload.get("data") or []:
        if not isinstance(row, dict):
            continue
        ts = row.get("announcement_datetime")
        if isinstance(ts, (int, float)) and ts >= now:
            rows.append(row)
    rows.sort(key=lambda r: r["announcement_datetime"])
    return rows


def headline_rank(row):
    importance = IMPORTANCE_RANK.get(row.get("event_importance"), 0)
    known = list(SHORT_NAMES)
    release = row.get("release")
    position = known.index(release) if release in known else len(known)
    return (importance, -position)


def pick_headline(rows):
    floor = IMPORTANCE_RANK.get(MIN_IMPORTANCE, 1)
    for row in rows:
        if IMPORTANCE_RANK.get(row.get("event_importance"), 0) >= floor:
            # Several releases can share a timestamp (e.g. CPI and core CPI);
            # prefer the most important one at that time.
            same_time = [r for r in rows if r["announcement_datetime"] == row["announcement_datetime"]]
            return max(same_time, key=headline_rank)
    return rows[0] if rows else None


def format_value(value, unit):
    if not isinstance(value, (int, float)):
        return str(value)
    text = "%.2f" % value
    if text.endswith("0"):
        text = text[:-1]
    return text + "%" if unit and "%" in unit else text


def latest_values():
    results = []
    delay = None
    for indicator, label in LATEST_INDICATORS:
        try:
            payload = get_json("/announcements/%s/%s?limit=1" % (CURRENCY, indicator))
        except ApiError as exc:
            results.append((label, None, str(exc)))
            continue
        if (payload.get("freemium_delay") or {}).get("applied"):
            current = payload["freemium_delay"]
            if delay is None or current.get("withheld_count", 0) > delay.get("withheld_count", 0):
                delay = current
        data = payload.get("data") or []
        if not data:
            results.append((label, None, "no recent data"))
            continue
        row = data[0] if isinstance(data[0], dict) else {}
        unit = (payload.get("value_metadata") or {}).get("source_unit", "")
        value = format_value(row.get("val"), unit)
        change = row.get("change_from_previous")
        if isinstance(change, (int, float)) and abs(change) >= 0.005:
            value += " (%+.2f)" % change
        released = row.get("announcement_datetime")
        when = datetime.fromtimestamp(released).strftime("%d %b %Y") if released else row.get("date", "")
        results.append((label, value, "released %s" % when, row.get("source_url")))
    return results, delay


def main():
    now = time.time()
    try:
        rows = upcoming_releases(now)
    except ApiError as exc:
        print("FXMacroData unavailable | color=#999999")
        print("---")
        print(clean(exc))
        if CURRENCY != "usd" and not API_KEY:
            print("%s needs an API key (set it in the plugin settings) | href=%s/subscribe" % (CURRENCY.upper(), SITE_URL))
        print("Refresh | refresh=true")
        print("Documentation | href=%s" % DOCS_URL)
        return

    headline = pick_headline(rows)
    if headline:
        wait = countdown(headline["announcement_datetime"] - now)
        print("%s in %s" % (clean(short_name(headline)), wait))
    else:
        print("%s: no scheduled releases" % CURRENCY.upper())
    print("---")

    print("Upcoming %s releases | size=12" % CURRENCY.upper())
    for row in rows[:UPCOMING_COUNT]:
        importance = row.get("event_importance") or "low"
        line = "%s  %s  [%s]" % (local_time(row["announcement_datetime"]), clean(row.get("name") or row.get("release")), importance)
        params = ["font=Menlo", "size=12"]
        if importance in IMPORTANCE_COLOR:
            params.append("color=%s" % IMPORTANCE_COLOR[importance])
        print("%s | %s" % (line, " ".join(params)))
    if not rows:
        print("Nothing scheduled | disabled=true")

    print("---")
    print("Latest %s prints | size=12" % CURRENCY.upper())
    values, delay = latest_values()
    for item in values:
        label, value, detail = item[0], item[1], item[2]
        if value is None:
            print("%s: unavailable (%s) | color=#999999" % (label, clean(detail)))
            continue
        line = "%s: %s, %s" % (label, value, detail)
        if len(item) > 3 and item[3]:
            print("%s | href=%s" % (clean(line), item[3]))
        else:
            print(clean(line))

    if delay:
        print("---")
        withheld = delay.get("withheld_count") or 0
        if withheld:
            print("%d new release(s) withheld by the free-tier delay | color=#e8a33d" % withheld)
        print("Free access is delayed by %s minutes | color=#999999" % delay.get("delay_minutes", 15))
        if delay.get("subscribe_url"):
            print("--Get real-time data with an API key | href=%s" % delay["subscribe_url"])

    print("---")
    print("Release calendar and API docs | href=%s" % DOCS_URL)
    print("Refresh | refresh=true")


if __name__ == "__main__":
    main()
