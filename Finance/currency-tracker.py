#!/usr/bin/python
import urllib2
import json

# Write here the currencies you want to see
#Comparaison currency
currFrom="EUR"
#Array of tracking currencies
currTo=["CAD"]

urlParams=[currFrom]
urlParams.extend(currTo)
url="http://api.fixer.io/latest?symbols="+",".join(urlParams)

result=urllib2.urlopen(url).read()

jsonCurr=json.loads(result)

rates=jsonCurr["rates"]
keys=rates.keys();

for key in reversed(keys):
	print key + ": " + str(rates[key])
