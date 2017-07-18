#!/usr/local/bin/python
# -*- coding: utf-8 -*-
# <bitbar.title>GWR Wifi Status</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Adam Marsh</bitbar.author>
# <bitbar.author.github>Adam2Marsh</bitbar.author.github>
# <bitbar.desc>Shows the WiFi Status when on a GWR train.</bitbar.desc>
# <bitbar.image>http://gwr.passengerwifi.com/library/images/logo-gwr%402x.png</bitbar.image>
# <bitbar.dependencies>GWR-Wifi,python,dryscrape</bitbar.dependencies>

# GWR Passenger Wifi Page
url = "http://gwr.passengerwifi.com/connected.php"

import dryscrape
import re
# import urllib2

try:
    # result = urllib2.urlopen(url, timeout = 5).read()
    session = dryscrape.Session()
    session.visit(url)
    result = session.body();
    print "🚂"
    print "---"

    try:
        signalStrength = re.search('<div class=\"guagevalue\" id=\"signalvalue\">(\d+%)<\/div>', result);
        print "Signal Strength:"
        print signalStrength.group(1) + "| href=" + url
    except:
        print "❌"
        print "---"

    try:
        howBusy = re.search('<div class=\"guagevalue\">([\w\s]+)<\/div>', result);
        print "How Busy:"
        print howBusy.group(1) + "| href=" + url
    except:
        print "❌"
        print "---"

    try:
        location = re.search('<div class=\"description\">[\t\n\r]\s+<h2>([\w\s]+)<\/h2>', result);
        print "Location:"
        print location.group(1) + "| href=" + url
    except:
        print "❌"
        print "---"
except:
    print "❌"
    print "---"
    print "GWR Passenger Wifi Not Available | color=red"
    print "---"
