#!/usr/bin/env PYTHONIOENCODING=UTF-8 python3

# NOTE: Please install dependencies: `pip3 install bimmer_connected` (or `pip3 install bimmer_connected[china]` for china region)

# <xbar.title>BMW Charging Status</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>zakx</xbar.author>
# <xbar.author.github>zakx</xbar.author.github>
# <xbar.desc>Shows your BMW's ConnectedDrive charging status.</xbar.desc>
# <xbar.dependencies>python,bimmer_connected</xbar.dependencies>
# <xbar.image>https://c.zk.je/battery-status-26-percent-114km-remaining-RJjD.png</xbar.image>
#
# <xbar.var>string(BMW_CONNECTED_DRIVE_USERNAME="bmwuser@example.invalid"): Your ConnectedDrive username</xbar.var>
# <xbar.var>string(BMW_CONNECTED_DRIVE_PASSWORD="password"): Your ConnectedDrive password</xbar.var>
# <xbar.var>select(BMW_CONNECTED_DRIVE_REGION="rest_of_world"): Which server region to use [north_america, rest_of_world, china]</xbar.var>
# <xbar.var>string(BMW_CONNECTED_DRIVE_VIN=""): Your car's full VIN, if you have more then one. You can leave it empty otherwise.</xbar.var>

import base64
import datetime
import os
import asyncio
import sys
import traceback

try:
    from bimmer_connected.account import MyBMWAccount
    from bimmer_connected.api.regions import get_region_from_name
    from bimmer_connected.vehicle import VehicleViewDirection
    from bimmer_connected.vehicle.fuel_and_battery import ChargingState
except ImportError as e:
    print(":warning: Error | emojize=true")
    print("---")
    print("Error: please install bimmer_connected python module.")
    print("To install it, run: pip3 install bimmer_connected")
    print("To install it for china region, run: pip3 install bimmer_connected[china]")
    print("If that doesn't fix it, please open an issue.")
    print("---")
    print("Raw error:")
    print(e)
    print(traceback.format_exc())
    print("---")
    print("Refresh | refresh=true")
    sys.exit(0)

BMW_CONNECTED_DRIVE_USERNAME = os.getenv("BMW_CONNECTED_DRIVE_USERNAME")
BMW_CONNECTED_DRIVE_PASSWORD = os.getenv("BMW_CONNECTED_DRIVE_PASSWORD")
BMW_CONNECTED_DRIVE_REGION = os.getenv("BMW_CONNECTED_DRIVE_REGION")
BMW_CONNECTED_DRIVE_VIN = os.getenv("BMW_CONNECTED_DRIVE_VIN")

LOGO = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAABX1BMVEV0dHT///9ubm5vb29JbYdwcHALY6QrapgqZpYAZrGmkH6cinssapgkZZk3a5GXl5cfaZjkoGtlb3ZxcXFycnJwcHBvb29wcHBwcHBwcHBwcHBwcHBwcHBxcXFwcHBwcHBvb29wcHBxcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBycnF1cW5rcHRvcHBxcXBwcHBwcHBwcHBzc3NwcHBwcHBxcHAUaKYEZ68IZ61TboJzcW9wcHBwcHBwcHBwcHBwcHBxcHA0a5MBZrEAZrEMZ6p+cWdxcXFxcXFvb28sa5kCZrAAZrEAZrJ+fn5ycnJxcXFwcHBycG8AZrMAZrEAZrF+cWdzcG9wcHBwcHC1h2QVaKcEZ68AZrEAZrEAZrEOZ6lQbYNkbneLg360hmRxcXFzcW8AZ7JxcXFycnJ7e3sAZrEAZ7Jvb3CAgIBzc3MAZ7JwcHBwcHBwcHAAZrH////TSxzlAAAAc3RSTlMAAAAAAAAAAAAAAAAAAAAAAAAAAAlCc3+FhnxzQRllWyJAkZUxIl1pfXkcL3Z+JzpkcAmPkGwxfpxIOSuRiFxGkInU5iAON2IkQdfjGwI3KncLeOUaDzQLdQEHl+fNMiBBCwEBdDV5HTQC0ygkAStJZzdyTLX+NwAAAAFiS0dEAf8CLd4AAAAHdElNRQflAwEXLBPRp6KhAAABB0lEQVQY0xXPh1qCUBgG4B+MzNwFNLEt7WG2i7ZRUB0CAgFnOXIUeO7/eTpewDdeAIpmOX5qemZ2bp6lKQAqJGQWFpeWV1bXsqIQooAW1jc2R7a2d3aZvf0DgYaceJg/Oj45PTu/uLzKiDngpOub2zt8//BYeMoXshzw8ujzC8aKGn59e2dkHpA29oEx1o1I2PwctxDYRcf1FL1UNtRKtVa3wa7XqhXVKJd0xXOdr29AVrRhxiKGTnLNuIZIKdNq/8RUBeNON9HjyWyy/9s2Kx7u/KXSEgesHzD9VqPqNrupicBnyfVBkGSiNSeeSAcDcp3gfEm26kWtJ/kCM+QO+ci2Ec8R/uQ/WgA6IA6KdS0AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjEtMDMtMDFUMjM6NDE6MDIrMDA6MDCCqrT8AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIxLTAyLTI4VDExOjE0OjI3KzAwOjAwtgzNTQAAAABJRU5ErkJggg=="

account = MyBMWAccount(
    BMW_CONNECTED_DRIVE_USERNAME,
    BMW_CONNECTED_DRIVE_PASSWORD,
    get_region_from_name(BMW_CONNECTED_DRIVE_REGION),
)
asyncio.run(account.get_vehicles())

if not BMW_CONNECTED_DRIVE_VIN:
    account_vins = []
    for vehicle in account.vehicles:
        account_vins.append(vehicle.vin)
    # we'll just pick the first one
    BMW_CONNECTED_DRIVE_VIN = account_vins[0]

vehicle = account.get_vehicle(BMW_CONNECTED_DRIVE_VIN)

image_data = asyncio.run(vehicle.get_vehicle_image(VehicleViewDirection.FRONTSIDE))
b64image = base64.b64encode(image_data)

brand = vehicle.brand
car_name = vehicle.name
mileage = vehicle.mileage
charging = vehicle.fuel_and_battery.charging_status
remaining_charge_time = 0
hv_charge_level = vehicle.fuel_and_battery.remaining_battery_percent
fuel_percent = vehicle.fuel_and_battery.remaining_fuel_percent
electric_range = vehicle.fuel_and_battery.remaining_range_electric
topstring = "BMW"
charging_menu_str = ""

if charging is ChargingState.CHARGING:
    if remaining_charge_time:
        topstring = (
            f":electric_plug: {hv_charge_level}%, full @ {remaining_charge_time:%H:%M}"
        )
        remaining = remaining_charge_time - datetime.datetime.now()
        remaining_str = "h".join(str(remaining).split(":")[:2]) + "m"
        charging_menu_str = f"Charging, {remaining_str} left | color=#FFFFFF"
    else:
        topstring = f":electric_plug: {hv_charge_level}% / {electric_range.value}{electric_range.unit}"
        charging_menu_str = "Charged (still connected) | color=#FFFFFF"
elif charging in (
    ChargingState.NOT_CHARGING,
    ChargingState.FINISHED_FULLY_CHARGED,
    ChargingState.FINISHED_NOT_FULL,
    ChargingState.INVALID,
    ChargingState.COMPLETE,
    ChargingState.FULLY_CHARGED,
    ChargingState.TARGET_REACHED,
):
    topstring = (
        f":battery: {hv_charge_level}% / {electric_range.value}{electric_range.unit}"
    )
elif charging in (ChargingState.WAITING_FOR_CHARGING, ChargingState.PLUGGED_IN):
    topstring = f":alarm_clock: {hv_charge_level}%, waiting…"
elif charging is ChargingState.ERROR:
    topstring = ":warning: Charging Error"

print(f"{topstring} | emojize=true templateImage=" + LOGO)
print("---")
print("| image=" + b64image.decode("ascii"))
print(f"{brand} {car_name} | color=#FFFFFF")
if charging_menu_str:
    print(charging_menu_str)
print(f"Mileage: {mileage.value} {mileage.unit} | color=#FFFFFF")
if fuel_percent:
    print(
        f"Fuel level: {fuel_percent} % (~{vehicle.fuel_and_battery.remaining_fuel.value}{vehicle.fuel_and_battery.remaining_fuel.unit}), ≙ {vehicle.fuel_and_battery.remaining_range_fuel.value} {vehicle.fuel_and_battery.remaining_range_fuel.unit} | color=#FFFFFF"
    )
if charging_menu_str and fuel_percent:
    print(
        f"Combined range: {vehicle.fuel_and_battery.remaining_range_total.value} {vehicle.fuel_and_battery.remaining_range_total.unit} | color=#FFFFFF"
    )
print("Refresh | refresh=true")
