#!/usr/bin/env python
# <bitbar.title>SBCDash</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>phntxx</bitbar.author>
# <bitbar.author.github>phntxx</bitbar.author.github>
# <bitbar.desc>Shows the CPU temperature, RAM and space usage of your SBC.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://phntxx.github.io/sbcdash</bitbar.abouturl>

#import required modules
import urllib2
import json

# Welcome to the SBCDash-Client for BitBar!
# For usage and prerequisite information, visit https://phntxx.github.io/sbcdash-server
# This Plugin requires a Single-Board Computer (such as the Raspberry Pi)
# and the sbcdash-server script installed on it.

# The IPAddrs-Variable is required for storing the IP-Addresses of your SBCs.
# Add your IP-Addresses in this variable in the following format:
# "http://<IP Address and path to sbcdash-server>/api.php"
# If you've set a password, add it by appending it to the IP-Address.

# Example (without password):
# "http://172.222.69.199/sbcdash-server/api.php"
# Example (with password):
# "http://172.222.69.199/sbcdash-server/api.php?password=helloworld"

IPAddresses = []

def getData(IP):
    response = urllib2.urlopen(IP)
    data = response.read()
    try:
        appData = json.loads(data)
        return {
            'temperature': appData['temperature'],
            'ram_free': appData['ram_free'] + "MB",
            'ram_used': appData['ram_used'] + "MB",
            'disk_free': appData['disk_free'] + "B",
            'disk_used': appData['disk_used'] + "B"
        }
    except:
        return None

output = []
for IP in IPAddresses:
    output.append(getData(IP))

print "sbcDash"
print "---"
for i in output:
    print "CPU: %sC" % (i['temperature'])
    print "RAM: %s free, %s used" % (i['ram_free'], i['ram_used'])
    print "Space: %s free, %s used" % (i['disk_free'], i['disk_used'])
    print "---"
