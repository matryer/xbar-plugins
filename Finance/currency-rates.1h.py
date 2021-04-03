#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# <xbar.title>Currency Rates</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Parvez</xbar.author>
# <xbar.author.github>parvez</xbar.author.github>
# <xbar.desc>Displays Currency Rates</xbar.desc>
# <xbar.image>http://i.imgur.com/Q1PuzbN.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://github.com/parvez/bitbar-plugins</xbar.abouturl>
#
# by Parvez

import urllib2
rateRequest = urllib2.Request(url='http://wu-converter.apple.com/dgw?apptype=finance', data='<?xml version="1.0" encoding="utf8"?><request app="YGoAppleCurrencyWidget" appver="1.0" api="finance" apiver="1.0.0"><query id="0" type="convertcurrency"><from/><to/><amount/></query></request>',headers={'Content-Type': 'text/xml'})
rateResponse = urllib2.urlopen(rateRequest).read()

from xml.dom import minidom
xmlResponse = minidom.parseString(rateResponse)
xmlConversionList = xmlResponse.getElementsByTagName('conversion')

list = []
for item in xmlConversionList:
    nValue1 = item.childNodes[1].firstChild.nodeValue
    nValue2 = str(round(float(item.childNodes[3].firstChild.nodeValue), 2))
    if nValue1 == "INR":
        primaryValue = 'Rs ' + nValue2
    elif nValue1 == "USD":
        discard = ''
    else:
        list.append(nValue1 + ': ' + nValue2)

print primaryValue
print ("---")

list.sort()
for item in list:
    print item + ' | font=Monaco size=11'
