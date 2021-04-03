#!/usr/bin/env python

# <xbar.title>USB Device Details</xbar.title>
# <xbar.author>Raemond Bergstrom-Wood</xbar.author>
# <xbar.author.github>RaemondBW</xbar.author.github>
# <xbar.desc>Displays the device details user input usb devices</xbar.desc>
# <xbar.version>1.0</xbar.version>

print "USB"
print '---'
import plistlib
import subprocess
def findDevices(itemList):
    for device in itemList:
        if '_items' in device:
            findDevices(device['_items'])
        elif 'Built-in_Device' in device and device['Built-in_Device'] == 'Yes':
            continue
        else:
            print "Name:\t\t\t" + device['_name'] + '| bash=/usr/bin/open param1="/Applications/Utilities/System Information.app" terminal=false'
            if 'manufacturer' in device:
                print "Manufacturer:\t" + device['manufacturer'] + '| bash=/usr/bin/open param1="/Applications/Utilities/System Information.app" terminal=false'
            print '---'

usbPlist = subprocess.check_output(['system_profiler', '-xml', 'SPUSBDataType'])
usbInfo = plistlib.readPlistFromString(usbPlist)
findDevices(usbInfo)
