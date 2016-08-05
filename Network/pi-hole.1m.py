#!/usr/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>Pi-hole status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Siim Ots</bitbar.author>
# <bitbar.author.github>siimots</bitbar.author.github>
# <bitbar.desc>Show your Pi-Hole (Raspberry Pi adblocker) status. Todays blocked ads, DNS queries and number of domains in block list.</bitbar.desc>
# <bitbar.image>http://gis.ee/files/pihole-bitbar.png</bitbar.image>
# <bitbar.dependencies>pi-hole,python</bitbar.dependencies>

import urllib2
import json

# If you have UTF-8 problems then uncomment next 3 lines
#import sys
#reload(sys)
#sys.setdefaultencoding("utf-8")

# Change to Your Pi-Hole Admin Console URL
pihole = "http://192.168.0.101/admin/"

try:
    url = pihole + "api.php"
    result = urllib2.urlopen(url, timeout = 5).read()
    json = json.loads(result)
    print "🌍"
    print "---"
    print "Ads blocked:"
    print json['ads_blocked_today'] + "| href=" + pihole
    print "DNS queries:"
    print json['dns_queries_today'] + "| href=" + pihole
    print "Domain list size:"
    print json['domains_being_blocked'] + "| href=" + pihole
    print "---"
    
except:
    print "❌"
    print "---"
    print "Pi-Hole not found | color=red"
    print "---"
