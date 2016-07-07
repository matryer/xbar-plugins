#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <bitbar.title>Adobe Flash Versions</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Parvez</bitbar.author>
# <bitbar.author.github>parvez</bitbar.author.github>
# <bitbar.desc>Displays current Adobe Flash Version for Mac</bitbar.desc>
# <bitbar.image>http://i.imgur.com/kxWNcl4.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/parvez/bitbar-plugins</bitbar.abouturl>
#
# by Parvez

from urllib import urlopen
from xml.dom import minidom
import json

# Check current version of Adobe Flash using online API from Adobe.
# Platform: Macintosh
# Arch: x86-64
# Returns JSON with download information for PPAPI and NPAPI
adobeCheckJsonUrl = urlopen('https://get.adobe.com/flashplayer/webservices/json/?platform_type=Macintosh&platform_dist=&platform_arch=x86-64&platform_misc=&browser_arch=&browser_type=&browser_vers=&browser_dist=&eventname=flashplayerotherversions').read()
adobeCheckJson = json.loads(adobeCheckJsonUrl)

# To check Current version of NPAPI, read plist file stored in plugins directory
currentVersionNPAPIxml = minidom.parse('/Library/Internet Plug-Ins/Flash Player.plugin/Contents/version.plist').getElementsByTagName('string')
# Store NPAPI versions for future comparison
currentVersionNPAPI = currentVersionNPAPIxml[0].childNodes[0].nodeValue
newVersionNPAPI = adobeCheckJson[0]['Version']

# To check Current version of PPAPI (PepperFlash), read json stored in plugins directory
with open('/Library/Internet Plug-Ins/PepperFlashPlayer/manifest.json') as json_file:
    currentVersionPPAPIjson = json.load(json_file)
# Store PPAPI versions for future comparison
currentVersionPPAPI = currentVersionPPAPIjson['version']
newVersionPPAPI = adobeCheckJson[1]['Version']

# Compare with installed version to show appropriate alert icon
if currentVersionNPAPI == newVersionNPAPI and currentVersionPPAPI == newVersionPPAPI:
    print ("✓")
else:
    print ("⚡️")
print ("---")
# Show current NPAPI installed version and online available version with link to the dmg
print ('Current NPAPI: ' + currentVersionNPAPI)
print (adobeCheckJson[0]['Name'] + ': ' + adobeCheckJson[0]['Version'] + '| href:' + adobeCheckJson[0]['download_url'])
print ("---")
# Show current PPAPI installed version and online available version with link to the dmg
print ('Current PPAPI: ' + currentVersionPPAPI)
print (adobeCheckJson[1]['Name'] + ': ' + adobeCheckJson[1]['Version'] + '| href:' + adobeCheckJson[1]['download_url'])
