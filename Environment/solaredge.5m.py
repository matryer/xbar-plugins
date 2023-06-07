#!/usr/bin/python3
# -*- coding: utf-8 -*-
# <xbar.title>SolarEdge Monitoring</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Shaun Grady</xbar.author>
# <xbar.author.github>shaungrady</xbar.author.github>
# <xbar.desc>Displays SolarEdge inverter power and energy generation data from your solar energy system. Also calculates system efficiency for the current day and total CO2 offset.</xbar.desc>
# <xbar.image>https://i.imgur.com/wPRb9dj.png</xbar.image>
# <xbar.dependencies>solaredge,python</xbar.dependencies>
# <xbar.var>string(SITE_ID): Please provide your SolarEdge installation SiteId</xbar.var>
# <xbar.var>string(API_KEY): Please provide your SolarEdge installation API Key (https://www.solaredge.com/node/88689)</xbar.var>
# <xbar.var>string(BATTERY_PRESENT): Does your SolarEdge installation include a battery? Y/N</xbar.var>
# <xbar.var>string(FONT_SIZE): Select a font size (Default 13)</xbar.var>

####################
# Import Statements
import urllib.request, urllib.error, urllib.parse
import json
import os

####################
# User Configuration
solaredge_site_id = os.getenv("SITE_ID", "")
solaredge_api_key = os.getenv("API_KEY", "")
font_size = os.getenv("FONT_SIZE", "13")

# Optional. Set to 0 to disable. Total panel DC watt capacity
system_wattage = 5460

# Optional. Set to 0 to disable. Find CO2 lbs/MWh for your subregion in the PDF linked below.
# (Use the "Non-baseload output emission rates" figure for your subregion)
# https://www.epa.gov/sites/production/files/2015-10/documents/egrid2012_summarytables_0.pdf
co2_lbs_per_MWh = 0  # California

# Optional. Set either as empty string to disable.
awake_icon = "☀︎"
asleep_icon = "☾"
battery_icon = "🔋"
low_battery_icon = "🪫"

# Optional. Set to "n" to disable, "y" to enable. Display current battery charge state
battery_present = os.getenv("BATTERY_PRESENT", "")


##############
# Begin Script
# Build Base URLs
overview = "https://monitoringapi.solaredge.com/site/" + solaredge_site_id + "/overview?api_key=" + solaredge_api_key
power = "https://monitoringapi.solaredge.com/site/" + solaredge_site_id + "/currentPowerFlow?api_key=" + solaredge_api_key

# Handle empty SiteId or API Key
if solaredge_site_id == "":
    raise SystemExit("SiteId is required")
if solaredge_api_key == "":
    raise SystemExit("API Key is required, see here: https://www.solaredge.com/node/88689")

# Functions
def convertKwToW(kW):
    kW = float(kW)
    return kW * 1000

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

try:
    overviewResult = urllib.request.urlopen(overview, timeout=10).read()
    jsonOverview = json.loads(overviewResult)
    powerResult = urllib.request.urlopen(power, timeout=10).read()
    jsonPower = json.loads(powerResult)
except Exception as err:
    print((asleep_icon + " <err>"))
    print("---")
    raise SystemExit(err)

raw_power = jsonOverview['overview']['currentPower']['power']
raw_energy = jsonOverview['overview']['lastDayData']['energy']

if battery_present == "Y":
    battery_status = jsonPower['siteCurrentPowerFlow']['STORAGE']['status']
    battery_discharge_power = jsonPower['siteCurrentPowerFlow']['STORAGE']['currentPower']
    battery_charge_level = jsonPower['siteCurrentPowerFlow']['STORAGE']['chargeLevel']

if system_wattage > 0:
    raw_efficiency = raw_energy / system_wattage

raw_energy_mtd = jsonOverview['overview']['lastMonthData']['energy']
raw_energy_ytd = jsonOverview['overview']['lastYearData']['energy']
raw_energy_total = jsonOverview['overview']['lifeTimeData']['energy']

inverter_load = jsonPower['siteCurrentPowerFlow']['LOAD']['currentPower']
inverter_grid_load = jsonPower['siteCurrentPowerFlow']['GRID']['currentPower']

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
if battery_present == "Y":
    combinedPower = formatWatts(raw_power + convertKwToW(battery_discharge_power))
energy = formatWatts(raw_energy, "h")
if system_wattage > 0:
    efficiency = "%.2f" % raw_efficiency + " Wh/W"

# Formulate PV output string
if battery_present == "Y":
    if raw_energy == 0 and raw_power == 0 and battery_discharge_power == 0:
        toolbar_output = "— Wh"
    elif raw_power == 0 and battery_discharge_power == 0:
        toolbar_output = energy
    else:
        toolbar_output = energy + " @ " + combinedPower
else:
    if raw_energy == 0 and raw_power == 0:
        toolbar_output = "— Wh"
    elif raw_power == 0:
        toolbar_output = energy
    else:
        toolbar_output = energy + " @ " + power

# Battery Icon
if battery_present == "Y":
    if battery_charge_level > 25:
        battery_icon_prefix = battery_icon
    elif battery_charge_level <= 25:
        battery_icon_prefix = low_battery_icon
    else:
        battery_icon_prefix = ""

# Icon
if raw_power == 0 and asleep_icon:
    icon_prefix = asleep_icon + " "
elif raw_power > 0 and awake_icon:
    icon_prefix = awake_icon + " "
else:
    icon_prefix = ""


# Print the data
if battery_present == "Y":
    print((battery_icon_prefix + str(battery_charge_level) + "% " + icon_prefix + toolbar_output + "| font='SF Compact Text Regular'| size=" + font_size))
else:
    print((icon_prefix + toolbar_output + "| font='SF Compact Text Regular'"))

print("---")
print("⚡ " + (formatWatts(convertKwToW(inverter_load)) + " inverter power| href=https://monitoring.solaredge.com/") + "| size=" + font_size)
print("🔌 " + (formatWatts(convertKwToW(inverter_grid_load)) + " grid power| href=https://monitoring.solaredge.com/") + "| size=" + font_size)

if system_wattage > 0:
    print("---")
    print((efficiency + " efficiency | href=https://monitoring.solaredge.com/") + "| size=12")

print("---")
print((energy_mtd + " this month | href=https://monitoring.solaredge.com/") + "| size=" + font_size)
print((energy_ytd + " this year | href=https://monitoring.solaredge.com/") + "| size=" + font_size)
# If YTD and lifetime energy are within 1 kWh, consider them equal and
# suppress the total energy data from the dropdown menu
if raw_energy_total - raw_energy_ytd > 1000:
    print((energy_total + " lifetime | href=https://monitoring.solaredge.com/") + "| size=" + font_size)

if co2_lbs_per_MWh > 0:
    print("---")
    print((co2_offset + " CO₂ offset | href=https://monitoring.solaredge.com/") + "| size=" + font_size)

print("---")
print((jsonOverview['overview']['lastUpdateTime'] + "| size=" + font_size))
