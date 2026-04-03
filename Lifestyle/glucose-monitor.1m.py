#!/usr/bin/env python3
import os, urllib.request, json, hashlib

# <xbar.title>Glucose Monitor</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ahmad Anvari</xbar.author>
# <xbar.author.github>anvari</xbar.author.github>
# <xbar.desc>Displays real-time blood glucose levels using the Nightscout API.</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://anvari.org/</xbar.abouturl>
# <xbar.var>string(VAR_NIGHTSCOUT_URL=""): Your own Nightscout URL.</xbar.var>
# <xbar.var>string(VAR_TOKEN=""): Your own Nightscout API Token</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_HIGH=185): (optional) Custom high threshold for the range.</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_SLIGHTLY_HIGH=145): (optional) Custom slightly high threshold for the range.</xbar.var>
# <xbar.var>number(VAR_CUSTOM_RANGE_LOW=65): (optional) Custom low threshold for the range.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_HIGH="red"): (optional) Color to display when the value is above the high threshold.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_SLIGHTLY_HIGH="yellow"): (optional) Color to display when the value is above the slightly high threshold and below high threshold.</xbar.var>
# <xbar.var>string(VAR_RANGE_COLOR_LOW="red"): (optional) Color to display when the value is below the low threshold.</xbar.var>

# --- YOUR EXACT CONFIGURATION ---
NIGHTSCOUT_URL = os.environ.get("VAR_NIGHTSCOUT_URL","")
VAR_TOKEN = os.environ.get("VAR_TOKEN","")
API_SECRET = hashlib.sha1(VAR_TOKEN.encode('utf-8')).hexdigest()
HIGH_THRESHOLD = int(os.environ.get("VAR_CUSTOM_RANGE_HIGH", "180"))
LOW_THRESHOLD = int(os.environ.get("VAR_CUSTOM_RANGE_LOW", "70"))

# The specific endpoint for glucose data
URL = f"{NIGHTSCOUT_URL.rstrip('/')}/api/v1/entries/sgv.json?count=1"

# --- TERMINAL / SAFETY CHECK ---
# If running in terminal (empty vars), show a helpful message instead of crashing
if not NIGHTSCOUT_URL or "http" not in NIGHTSCOUT_URL:
    print("Nightscout URL Setup Needed")
    print("---")
    print("To run from CLI, use: export VAR_NIGHTSCOUT_URL='your_url'")
    import sys
    sys.exit(0) 

# Setup the request with your API Secret
req = urllib.request.Request(URL)
req.add_header("api-secret", API_SECRET)
req.add_header("Accept", "application/json")

try:
    with urllib.request.urlopen(req) as response:
        data = json.loads(response.read().decode())
        if data and len(data) > 0:
            entry = data[0]
            sgv = entry.get('sgv', 0)
            direction = entry.get('direction', 'Flat')

            # Determine color based on SGV value
            color = "white" # Default/In-range
            if sgv >= HIGH_THRESHOLD:
                color = "orange"
            elif sgv <= LOW_THRESHOLD:
                color = "red"

            arrows = {
                "DoubleUp": "⇈", "SingleUp": "↑", "FortyFiveUp": "↗",
                "Flat": "→", "FortyFiveDown": "↘", "SingleDown": "↓", "DoubleDown": "⇊"
            }
            arrow = arrows.get(direction, direction)

            # Print with xbar color parameter
            print(f"{sgv} {arrow} | color={color}")
        else:
            print("No Data")
except Exception as e:
    print("Glucose Monitor Error | color=red")
    print("---")
    print(f"Details: {e}")

'''
try:
    with urllib.request.urlopen(req) as response:
        # Get the first entry from the list
        data = json.loads(response.read().decode())
        if data and len(data) > 0:
            entry = data[0]
            sgv = entry.get('sgv', '??')
            direction = entry.get('direction', 'Flat')
            
            # Map Nightscout directions to arrows
            arrows = {
                "DoubleUp": "⇈", "SingleUp": "↑", "FortyFiveUp": "↗", 
                "Flat": "→", 
                "FortyFiveDown": "↘", "SingleDown": "↓", "DoubleDown": "⇊"
            }
            arrow = arrows.get(direction, direction)
            
            # This prints to your Mac Menu Bar
            print(f"{sgv} {arrow}")
        else:
            print("No Data")
except Exception as e:
    # Prints the error in the dropdown if it fails
    print("Glucose Monitor Error")
    print("---")
    print(f"Details: {e}")
'''
