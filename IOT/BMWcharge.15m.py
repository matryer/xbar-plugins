#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3
# -*- coding: utf-8 -*-

# <bitbar.title>BMW Charging Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>zakx</bitbar.author>
# <bitbar.author.github>zakx</bitbar.author.github>
# <bitbar.desc>Shows your BMW's ConnectedDrive charging status.</bitbar.desc>
# <bitbar.dependencies>python,bimmer_connected</bitbar.dependencies>
# <bitbar.image>http://c.zk.je/Screen-Shot-2021-03-02-02-06-10.45-aXuuYL26.png</bitbar.image>
#
# Please install dependencies: `pip3 install bimmer_connected`

# Please fill your ConnectedDrive account info:
BMW_CONNECTED_DRIVE_USERNAME = ""
BMW_CONNECTED_DRIVE_PASSWORD = ""
# Region. One of {north_america,china,rest_of_world}
BMW_CONNECTED_DRIVE_REGION = "rest_of_world"
# Your car's full VIN, if you have more then one. You can leave it empty otherwise.
BMW_CONNECTED_DRIVE_VIN = ""

LOGO = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAABX1BMVEV0dHT///9ubm5vb29JbYdwcHALY6QrapgqZpYAZrGmkH6cinssapgkZZk3a5GXl5cfaZjkoGtlb3ZxcXFycnJwcHBvb29wcHBwcHBwcHBwcHBwcHBwcHBxcXFwcHBwcHBvb29wcHBxcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBycnF1cW5rcHRvcHBxcXBwcHBwcHBwcHBzc3NwcHBwcHBxcHAUaKYEZ68IZ61TboJzcW9wcHBwcHBwcHBwcHBwcHBxcHA0a5MBZrEAZrEMZ6p+cWdxcXFxcXFvb28sa5kCZrAAZrEAZrJ+fn5ycnJxcXFwcHBycG8AZrMAZrEAZrF+cWdzcG9wcHBwcHC1h2QVaKcEZ68AZrEAZrEAZrEOZ6lQbYNkbneLg360hmRxcXFzcW8AZ7JxcXFycnJ7e3sAZrEAZ7Jvb3CAgIBzc3MAZ7JwcHBwcHBwcHAAZrH////TSxzlAAAAc3RSTlMAAAAAAAAAAAAAAAAAAAAAAAAAAAlCc3+FhnxzQRllWyJAkZUxIl1pfXkcL3Z+JzpkcAmPkGwxfpxIOSuRiFxGkInU5iAON2IkQdfjGwI3KncLeOUaDzQLdQEHl+fNMiBBCwEBdDV5HTQC0ygkAStJZzdyTLX+NwAAAAFiS0dEAf8CLd4AAAAHdElNRQflAwEXLBPRp6KhAAABB0lEQVQY0xXPh1qCUBgG4B+MzNwFNLEt7WG2i7ZRUB0CAgFnOXIUeO7/eTpewDdeAIpmOX5qemZ2bp6lKQAqJGQWFpeWV1bXsqIQooAW1jc2R7a2d3aZvf0DgYaceJg/Oj45PTu/uLzKiDngpOub2zt8//BYeMoXshzw8ujzC8aKGn59e2dkHpA29oEx1o1I2PwctxDYRcf1FL1UNtRKtVa3wa7XqhXVKJd0xXOdr29AVrRhxiKGTnLNuIZIKdNq/8RUBeNON9HjyWyy/9s2Kx7u/KXSEgesHzD9VqPqNrupicBnyfVBkGSiNSeeSAcDcp3gfEm26kWtJ/kCM+QO+ci2Ec8R/uQ/WgA6IA6KdS0AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjEtMDMtMDFUMjM6NDE6MDIrMDA6MDCCqrT8AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIxLTAyLTI4VDExOjE0OjI3KzAwOjAwtgzNTQAAAABJRU5ErkJggg=="

from bimmer_connected.account import ConnectedDriveAccount
from bimmer_connected.country_selector import get_region_from_name
from bimmer_connected.vehicle import VehicleViewDirection
from bimmer_connected.state import ChargingState

import base64
import datetime

account = ConnectedDriveAccount(
    BMW_CONNECTED_DRIVE_USERNAME,
    BMW_CONNECTED_DRIVE_PASSWORD,
    get_region_from_name(BMW_CONNECTED_DRIVE_REGION),
)
account.update_vehicle_states()

if not BMW_CONNECTED_DRIVE_VIN:
    account_vins = []
    for vehicle in account.vehicles:
        account_vins.append(vehicle.vin)
    # we'll just pick the first one
    BMW_CONNECTED_DRIVE_VIN = account_vins[0]

vehicle = account.get_vehicle(BMW_CONNECTED_DRIVE_VIN)

image_data = vehicle.get_vehicle_image(200, 80, VehicleViewDirection.FRONT)
b64image = base64.b64encode(image_data)

brand = vehicle.attributes["brand"]
car_name = vehicle.attributes["model"]
mileage = vehicle.state.mileage
charging = vehicle.state.charging_status
remaining_charge_time = 0
hv_charge_level = vehicle.state.charging_level_hv
fuel_percent = vehicle.state.vehicle_status.attributes.get("fuelPercent", 999)
electric_range = vehicle.state.remaining_range_electric
topstring = "BMW"
charging_menu_str = ""

if charging is ChargingState.CHARGING:
    remaining_charge_time = vehicle.state.charging_time_remaining
    remaining_str = "h".join(str(remaining_charge_time).split(":")[:2]) + "m"
    if remaining_charge_time:
        time_full_charge = datetime.datetime.now() + remaining_charge_time
        topstring = (
            f":electric_plug: {hv_charge_level}%, full @ {time_full_charge:%H:%M}"
        )
        charging_menu_str = f"Charging, {remaining_str} left | color=#FFFFFF"
    else:
        topstring = f":electric_plug: {hv_charge_level}% / {electric_range}km"
        charging_menu_str = "Charged (still connected) | color=#FFFFFF"
elif charging in (
    ChargingState.NOT_CHARGING,
    ChargingState.FINISHED_FULLY_CHARGED,
    ChargingState.FINISHED_NOT_FULL,
    ChargingState.INVALID,
):
    topstring = f":battery: {hv_charge_level}% / {electric_range}km"
elif charging is ChargingState.WAITING_FOR_CHARGING:
    topstring = f":alarm_clock: {hv_charge_level}%, waiting…"
elif charging is ChargingState.ERROR:
    topstring = ":warning: Charging Error"

print(f"{topstring} | emojize=true templateImage=" + LOGO)
print("---")
print("| image=" + b64image.decode("ascii"))
print(f"{brand} {car_name} | color=#FFFFFF")
if charging_menu_str:
    print(charging_menu_str)
print(f"Mileage: {mileage} km | color=#FFFFFF")
print(
    f"Fuel level: {fuel_percent} % (~{vehicle.state.remaining_fuel}l), ≙ {vehicle.state.remaining_range_fuel} km | color=#FFFFFF"
)
print(f"Combined range: {vehicle.state.remaining_range_total} km | color=#FFFFFF")
print("Refresh | refresh=true")
