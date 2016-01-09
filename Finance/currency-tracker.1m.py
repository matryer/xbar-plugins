#!/usr/bin/python
# -*- coding: utf-8 -*-
# Currency Tracker by Maxime Bertheau
# Have a look on the currencies from your menu bar !

import urllib2
import json

# Write here the currencies you want to see

#Base comparaison currency
currFrom="EUR"
#Array of tracked currencies
currTo=["CAD", "USD"]

urlParamTo = currTo[0]
if len(currTo) > 1:
    urlParamTo = ",".join(currTo)

url="http://api.fixer.io/latest?base=" + currFrom  +"&symbols=" + urlParamTo

result=urllib2.urlopen(url).read()

jsonCurr=json.loads(result)

rates=jsonCurr["rates"]
keys=rates.keys();

for key in reversed(keys):
	print key + ": " + str(rates[key])

print "---"
print "From: " + currFrom
