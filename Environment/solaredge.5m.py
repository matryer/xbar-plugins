#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>SolarEdge Monitoring</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
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

if solaredge_site_id == "" or solaredge_api_key == "":
    raise SystemExit("Site ID/API Key Required")

import urllib2
import json

overview = "https://monitoringapi.solaredge.com/site/" + solaredge_site_id + "/overview?api_key=" + solaredge_api_key

result = urllib2.urlopen(overview, timeout = 5).read()
json = json.loads(result)

raw_power = json['overview']['currentPower']['power']
raw_energy = json['overview']['lastDayData']['energy']

if system_wattage > 0:
    raw_efficiency = raw_energy / system_wattage

raw_energy_mtd = json['overview']['lastMonthData']['energy']
raw_energy_ytd = json['overview']['lastYearData']['energy']
raw_energy_total = json['overview']['lifeTimeData']['energy']

energy_mtd = str(int(round(raw_energy_mtd / 1000))) + " kWh"
energy_ytd = str(int(round(raw_energy_ytd / 1000))) + " kWh"
energy_total = str(int(round(raw_energy_total / 1000))) + " kWh"

if co2_lbs_per_MWh > 0:
    co2_lbs_offset = str(int(round((raw_energy_total / 1000000) * co2_lbs_per_MWh))) + " lbs"

# Human-friendly power, energy, efficiency strings
if raw_power < 900:
    power = str(int(round(raw_power))) + " W"
else:
    power = "%.1f" % (raw_power / 1000) + " kW"

if raw_energy < 900:
    energy = str(int(round(raw_energy))) + " Wh"
else:
    energy = "%.1f" % (raw_energy / 1000) + " kWh"

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
    print(co2_lbs_offset + " CO₂ offset | href:https://monitoring.solaredge.com/")

# print("---")
# print(json['overview']['lastUpdateTime'] + " | size:11")
