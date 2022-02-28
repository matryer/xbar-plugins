#!/usr/local/bin/python3

# <xbar.title>Watt Meter</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Eric Ripa</xbar.author>
# <xbar.author.github>eripa</xbar.author.github>
# <xbar.desc>Show current watt drain, tries to be smart and highlight high power usage</xbar.desc>
# <xbar.image>http://i.imgur.com/blj2KCP.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>

from plistlib import loads
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
    },
    6: {
        "MacBook Pro": {
            "high": 80,
            "low": 30
        }
    }
}

def parse_system_profiler():
    output = subprocess.check_output(["system_profiler", \
                                     "-xml", "SPPowerDataType", "SPHardwareDataType"])
    plist = loads(output)
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
    print("%.1fW | color=%s" % (machine['current_watt'], color))
    print("---")
    print("Refresh (current interval: %s) | refresh=true" % refresh_interval)

if __name__ == '__main__':
    main()
