#!/usr/bin/env python3
# <xbar.title>Ambient Weather Menu</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.desc>Shows temperature and selected sensors from your AmbientWeather.net station.</xbar.desc>
# <xbar.author>Ben Guthro</xbar.author>
# <xbar.dependencies>python3,requests</xbar.dependencies>
# <xbar.image>https://ambientweather.net/assets/images/logo-ambientweather.svg</xbar.image>
# <xbar.var>string(API_KEY="YOUR_API_KEY_FROM_AMBIENT_WEATHER"): Ambient Weather API Key (optional if using ~/.ambient_config.json)</xbar.var>
# <xbar.var>string(APP_KEY=""): Ambient Weather Application Key (optional if using ~/.ambient_config.json)</xbar.var>
# <xbar.var>string(SENSORS_JSON=""): JSON array of sensor objects, e.g. [{"key":"tempf","name":"Outside","unit":"°F"},{"key":"humidity","name":"Humidity","unit":"%"}]</xbar.var>

import json
import os
import sys
from datetime import datetime, timezone
import tempfile
import subprocess

try:
    import requests
except Exception:
    print("Ambient Weather: missing 'requests' module")
    print("---")
    print("Install: pip3 install requests")
    print(sys.executable)
    sys.exit(0)


PLUGIN_DIR = os.path.dirname(os.path.abspath(__file__))
VARS_PATH = os.path.join(PLUGIN_DIR, os.path.basename(__file__) + ".vars.json")


def load_config():
    api_key = os.environ.get("API_KEY", "")
    app_key = os.environ.get("APP_KEY", "")

    sensors = []
    raw_sensors = os.environ.get("SENSORS_JSON") or os.environ.get("SENSORS")
    if raw_sensors:
        try:
            parsed = json.loads(raw_sensors)
            if isinstance(parsed, list):
                sensors = parsed
        except Exception:
            # If parsing fails, keep sensors from config file
            pass
    return {"api_key": api_key, "application_key": app_key, "sensors": sensors}


def fetch_last_data(api_key: str, app_key: str):
    url = "https://api.ambientweather.net/v1/devices"
    params = {"apiKey": api_key, "applicationKey": app_key}
    try:
        r = requests.get(url, params=params, timeout=5)
        r.raise_for_status()
        devices = r.json() or []
        if not devices:
            return None
        return devices[0].get("lastData", {})
    except Exception:
        return None


def fmt_temp(value):
    try:
        # Round to a tenth of a degree, drop trailing .0
        r = round(float(value), 1)
        return f"{int(r)}" if r.is_integer() else f"{r:.1f}"
    except Exception:
        return "N/A"


def main():
    # Support a clicked action to show raw data in a window
    if "--show-raw" in sys.argv:
        cfg = load_config()
        api_key = cfg.get("api_key")
        app_key = cfg.get("application_key")
        last = fetch_last_data(api_key, app_key) or {}
        try:
            # Pretty text of key/value pairs
            lines = [f"{k}: {v}" for k, v in last.items()]
            text = "\n".join(lines) if lines else "No data"
            with tempfile.NamedTemporaryFile("w", delete=False, suffix=".txt") as tf:
                tf.write(text)
                temp_path = tf.name
            # Open in TextEdit (scrollable window)
            subprocess.run(["open", "-a", "TextEdit", temp_path])
        except Exception:
            pass
        return

    # Support a clicked action to open/create the sidecar vars file
    if "--edit-config" in sys.argv:
        try:
            if not os.path.exists(VARS_PATH):
                default_vars = {
                    # Matches declared plugin vars; SwiftBar reads these into env
                    "API_KEY": os.environ.get("API_KEY", "YOUR_API_KEY_FROM_AMBIENT_WEATHER"),
                    "APP_KEY": os.environ.get("APP_KEY", "YOUR_APP_KEY_FROM_AMBIENT_WEATHER"),
                    "SENSORS_JSON": "[{\"key\": \"tempf\", \"name\": \"Outside\", \"unit\": \"\u00B0F\"}]"
                }
                with open(VARS_PATH, "w", encoding="utf-8") as f:
                    json.dump(default_vars, f, indent=2)
            subprocess.run(["open", "-a", "TextEdit", VARS_PATH])
        except Exception:
            pass
        return

    cfg = load_config()
    api_key = cfg.get("api_key")
    app_key = cfg.get("application_key")

    if not api_key or not app_key:
        print("Ambient Weather: configure API keys")
        print("---")
        print(f"Edit config: {VARS_PATH}")
        print("Or set plugin vars API_KEY and APP_KEY")
        # Provide direct action to open or create the config file
        print("Edit Config | bash=/bin/bash param1=-lc param2='/usr/bin/env python3 \"$PWD/ambient-weather.1m.py\" --edit-config' terminal=false")
        print("Refresh | refresh=true")
        return

    last = fetch_last_data(api_key, app_key) or {}

    # Pick the headline value: prefer tempf, else first configured sensor
    headline_value = last.get("tempf")
    unit = "°F"
    if headline_value is None and cfg.get("sensors"):
        first = cfg["sensors"][0]
        headline_value = last.get(first.get("key"))
        unit = first.get("unit", unit)

    headline = "N/A"
    if headline_value is not None:
        headline = f"{fmt_temp(headline_value)}{unit}"

    print(headline)
    print("---")
    print("Open Ambient Weather | href=https://ambientweather.net/dashboard")
    print("Refresh | refresh=true")
    print("---")

    # Sensor lines from config, if present
    sensors = cfg.get("sensors", [])
    if sensors:
        for s in sensors:
            key = s.get("key")
            name = s.get("name", key or "sensor")
            unit = s.get("unit", "")
            val = last.get(key)
            if val is None:
                print(f"{name}: - | color=#000000 bash=/usr/bin/true terminal=false")
            else:
                # Keep same rounding style as headline
                print(f"{name}: {fmt_temp(val)}{unit} | color=#000000 bash=/usr/bin/true terminal=false")
        print("---")

    # Optional: show timestamp if available
    ts = last.get("dateutc")
    if isinstance(ts, (int, float)):
        try:
            dt = datetime.fromtimestamp(ts / 1000.0, tz=timezone.utc)
            print(f"Updated: {dt.astimezone().strftime('%Y-%m-%d %H:%M:%S %Z')}")
        except Exception:
            pass

    # Action: Show raw key/value pairs in a window via TextEdit
    print("Show Available Sensors | bash=/bin/bash param1=-lc param2='/usr/bin/env python3 \"$PWD/ambient-weather.1m.py\" --show-raw' terminal=false")
    print("Edit Config | bash=/bin/bash param1=-lc param2='/usr/bin/env python3 \"$PWD/ambient-weather.1m.py\" --edit-config' terminal=false")


if __name__ == "__main__":
    main()
