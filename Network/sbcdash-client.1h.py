#!/usr/bin/env python
# <bitbar.title>SBCDash</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Bastian</bitbar.author>
# <bitbar.author.github>phntxx</bitbar.author.github>
# <bitbar.desc>Shows the CPU temperature, RAM and space usage of your SBC.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://phntxx.github.io/sbcdash</bitbar.abouturl>

#import required modules
import urllib2
import json

# Welcome to the SBCDash-Client for BitBar!
# For usage and prerequisite information, visit https://phntxx.github.io/sbcdash
# This Plugin requires a Single-Board-Computer (such as the Raspberry Pi)
# and the sbcdash-server script installed on it.
# The IPAddrs-Variable is required for storing the IP-Addresses of your SBCs.
# Add your IP-Addresses in this variable in the following format:
# "http://<IPADDRESS AND PATH TO SBCDASH>api.php?passwd=<PASSWORD, "NONE" IF NOT SET>"
# Example:
# "http://127.0.0.1/api.php?passwd=helloworld" (note the quotation marks around the IP-Address and the lack of them around the password!)

IPAddrs = []

# Don't touch this variable!
output = []

def getData(IP):
    response = urllib2.urlopen(IP)
    data = response.read()
    try:
        appData = json.loads(data)
        return {
            'temperature': appData['temperature'],
            'ram_free': appData['ram_free'] + "MB",
            'ram_used': appData['ram_used'] + "MB",
            'space_free': appData['space_free'] + "B",
            'space_used': appData['space_used'] + "B"
        }
    except:
        return None

for IP in IPAddrs:
    output.append(getData(IP))

print "SBCDash"
print "---"
for i in output:
    print "CPU: %sC" % (i['temperature'])
    print "RAM: %s free, %s used" % (i['ram_free'], i['ram_used'])
    print "Space: %s free, %s used" % (i['space_free'], i['space_used'])
    print "---"
