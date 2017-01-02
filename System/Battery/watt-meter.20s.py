#!/usr/local/bin/python3

# <bitbar.title>Watt Meter</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Eric Ripa</bitbar.author>
# <bitbar.author.github>eripa</bitbar.author.github>
# <bitbar.desc>Show current watt drain, tries to be smart and highlight high power usage</bitbar.desc>
# <bitbar.image>http://i.imgur.com/blj2KCP.png</bitbar.image>
# <bitbar.dependencies>python3</bitbar.dependencies>

from plistlib import readPlistFromBytes
import sys
import subprocess

# Currently only rough guestimates. Only somewhat tested number on MacBook Air.abs
# Feel free to update/tweak etc..
IMPACT = {
    2: {
        "MacBook Air": {
            "high": 20,
            "low": 10
        },
        "MacBook Pro": {
            "high": 50,
            "low": 20
        }
    },
    4: {
        "MacBook Pro": {
            "high": 70,
            "low": 30
        }
    }
}

def parse_system_profiler():
    output = subprocess.check_output(["/usr/sbin/system_profiler", \
                                     "-xml", "SPPowerDataType", "SPHardwareDataType"])
    plist = readPlistFromBytes(output)
    spbattery_info = plist[0]['_items'][0]

    machine = {
        "current_amperage": int(spbattery_info['sppower_current_amperage']),
        "current_voltage": int(spbattery_info['sppower_current_voltage']),
        "current_watt": abs(int(spbattery_info['sppower_current_voltage']) * \
            int(spbattery_info['sppower_current_amperage'])/1000/1000),
        "model": plist[1]['_items'][0]['machine_name'],
        "cores": plist[1]['_items'][0]['number_processors'],
    }
    return machine

def get_impact(machine):
    model = machine['model']
    cores = machine['cores']
    watt = machine['current_watt']
    low = IMPACT[cores][model]['low']
    high = IMPACT[cores][model]['high']
    if watt <= low:
        return "low"
    if watt >= high:
        return "high"
    return "mid"

def main():
    machine = parse_system_profiler()
    impact = get_impact(machine)
    color = "gray"
    if impact == "low":
        color = "green"
    if impact == "high":
        color = "red"
    refresh_interval = sys.argv[0].split('.')[2]
    print("%.1fW| color=%s size=12" % (machine['current_watt'], color))
    print("---")
    print("Refresh (current interval: %s) | refresh=true" % refresh_interval)

if __name__ == '__main__':
    main()
