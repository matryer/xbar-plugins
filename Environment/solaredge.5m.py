#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>SolarEdge Monitoring</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Shaun Grady</bitbar.author>
# <bitbar.author.github>shaungrady</bitbar.author.github>
# <bitbar.desc>Displays SolarEdge inverter power and energy generation data from your solar energy system. Also calculates system efficiency for the current day and total CO2 offset.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/W4ygbPd.png</bitbar.image>
# <bitbar.dependencies>solaredge,python</bitbar.dependencies>

####################
# User Configuration

solaredge_site_id = ""
solaredge_api_key = ""

# Optional. Set to 0 to disable. Total panel DC watt capacity
system_wattage = 0

# Optional. Set to 0 to disable. Find CO2 lbs/MWh for your subregion in the PDF linked below.
# (Use the "Non-baseload output emission rates" figure for your subregion)
# https://www.epa.gov/sites/production/files/2015-10/documents/egrid2012_summarytables_0.pdf
co2_lbs_per_MWh = 1018.87  # California

# Optional. Set either as empty string to disable.
awake_icon = "☀︎"
asleep_icon = "☾"


##############
# Begin Script

def formatWatts (Wh, unit_suffix=""):
    Wh = float(Wh)

    if Wh < 900:
        energy = Wh
        unit = "W"
    elif Wh < 900000:
        energy = Wh / 1000
        unit = "kW"
    elif Wh < 900000000:
        energy = Wh / 1000000
        unit = "MW"
    elif Wh < 900000000000:
        energy = Wh / 1000000000
        unit = "GW"

    if energy < 10:
        energy = round(energy, 2)
    elif energy < 100:
        energy = round(energy, 1)
    else:
        energy = int(round(energy))

    return str(energy) + " " + unit + unit_suffix

if solaredge_site_id == "" or solaredge_api_key == "":
    raise SystemExit("Site ID/API Key Required")

import urllib2
import json

overview = "https://monitoringapi.solaredge.com/site/" + solaredge_site_id + "/overview?api_key=" + solaredge_api_key

try:
    result = urllib2.urlopen(overview, timeout = 10).read()
    json = json.loads(result)
except Exception, err:
    print(asleep_icon + " <err>")
    print("---")
    raise SystemExit(err)

raw_power = json['overview']['currentPower']['power']
raw_energy = json['overview']['lastDayData']['energy']

if system_wattage > 0:
    raw_efficiency = raw_energy / system_wattage

raw_energy_mtd = json['overview']['lastMonthData']['energy']
raw_energy_ytd = json['overview']['lastYearData']['energy']
raw_energy_total = json['overview']['lifeTimeData']['energy']

# Handle strange API bug where energy total can be much less than YTD
if raw_energy_ytd > raw_energy_total:
    raw_energy_total = raw_energy_ytd

energy_mtd = formatWatts(raw_energy_mtd, "h")
energy_ytd = formatWatts(raw_energy_ytd, "h")
energy_total = formatWatts(raw_energy_total, "h")

if co2_lbs_per_MWh > 0:
    offset = (float(raw_energy_total) / 1000000) * co2_lbs_per_MWh
    unit = "lbs"
    if offset > 1900:
        co2_offset = offset / 2000
        unit = "tons"

    if co2_offset < 10:
        co2_offset = round(co2_offset, 2)
    elif co2_offset < 100:
        co2_offset = round(co2_offset, 1)
    else:
        o2_offset = "{:,}".format(int(round()))

    co2_offset = str(co2_offset) + " " + unit

# Human-friendly power, energy, efficiency strings
power = formatWatts(raw_power)
energy = formatWatts(raw_energy, "h")
if system_wattage > 0:
    efficiency = "%.2f" % raw_efficiency + " Wh/W"

# Formulate output string
if raw_energy == 0 and raw_power == 0:
    toolbar_output = "— Wh"
elif raw_power == 0:
    toolbar_output = energy
else:
    toolbar_output = energy + " @ " + power

# Icon
if raw_power == 0 and asleep_icon:
    icon_prefix = asleep_icon + " "
elif raw_power > 0 and awake_icon:
    icon_prefix = awake_icon + " "
else:
    icon_prefix = ""


# Print the data
print(icon_prefix + toolbar_output + "| font='SF Compact Text Regular'")

if system_wattage > 0:
    print("---")
    print(efficiency + " efficiency | href:https://monitoring.solaredge.com/")

print("---")
print(energy_mtd + " this month | href:https://monitoring.solaredge.com/")
print(energy_ytd + " this year | href:https://monitoring.solaredge.com/")
# If YTD and lifetime energy are within 1 kWh, consider them equal and
# suppress the total energy data from the dropdown menu
if raw_energy_total - raw_energy_ytd > 1000:
    print(energy_total + " lifetime | href:https://monitoring.solaredge.com/")

if co2_lbs_per_MWh > 0:
    print("---")
    print(co2_offset + " CO₂ offset | href:https://monitoring.solaredge.com/")

# print("---")
# print(json['overview']['lastUpdateTime'] + " | size:11")
