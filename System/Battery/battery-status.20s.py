#!/usr/local/bin/python2
# -*- coding: utf-8 -*-
# <bitbar.title>Battery remaining (Python)</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Eric Ripa</bitbar.author>
# <bitbar.author.github>eripa</bitbar.author.github>
# <bitbar.desc>Show battery charge percentage and time remaining</bitbar.desc>
# <bitbar.image>http://i.imgur.com/P6aNey5.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

from __future__ import print_function
import re
import sys
import subprocess

def parse_pmset():
    output = subprocess.check_output(["/usr/bin/pmset", "-g", "batt"])
    output = output.decode("utf-8").split('\n')
    regex = re.compile(r'^.*\s(?P<charge>\d+%);\s((?P<status>discharging|'
                       r'charging|finishing charge|charged);\s(?P<remain>\(no estimate\)|\d+:\d+) '
                       r'(remaining )?present|AC attached; not charging present): true$')

    battery = {
        "charge": "unknown",
        "status": "unknown",
        "remaining": "unknown",
    }

    battery_match = regex.match(output[1])
    if not battery_match:
        return battery

    battery["charge"] = battery_match.group("charge")
    battery["status"] = battery_match.group("status")
    battery["remaining"] = battery_match.group("remain")
    if battery["remaining"] == "(no estimate)":
        battery["remaining"] = "calculating.."
    if battery["remaining"] == "0:00" and battery["status"] == "charged":
        battery["remaining"] = "∞"
    return battery

def main():
    battery = parse_pmset()
    refresh_interval = sys.argv[0].split('.')[2]
    print("{}| size=12".format(battery["charge"]))
    print("---")
    print("Status: {}".format(battery["status"]))
    print("Remaining: {}".format(battery["remaining"]))
    print("Refresh (current interval: {}) | refresh=true".format(refresh_interval))

if __name__ == '__main__':
    main()
