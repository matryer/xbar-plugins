#!/usr/bin/env python3

# <xbar.title>Prayer Time</xbar.title>
# <xbar.version>1.1</xbar.version>
# <xbar.author>Kagan Inan</xbar.author>
# <xbar.author.github>kaaninan</xbar.author.github>
# <xbar.desc>Imsak ve aksam ezani saatlerine ne kadar kaldigini takip edin !</xbar.desc>

import urllib.request, urllib.error, urllib.parse
import json
from datetime import datetime
import time

# Istanbul, Turkey -> 9541
# For others: https://ezanvakti.herokuapp.com/
location_code = 9541

url_prayer = "https://ezanvakti.herokuapp.com/vakitler?ilce=%s" % location_code
url_time = "http://worldclockapi.com/api/json/utc/now"


# Get UTC time and convert to local time
result_time = urllib.request.urlopen(url_time).read()
jsonDataTime = json.loads(result_time)
now_server = jsonDataTime['currentDateTime']
utc = datetime.strptime(now_server[:-1], '%Y-%m-%dT%H:%M')
now_timestamp = time.time()
offset = datetime.fromtimestamp(now_timestamp) - datetime.utcfromtimestamp(now_timestamp)
now_server = utc + offset


# Get Prayer Time
result = urllib.request.urlopen(url_prayer).read()
jsonData = json.loads(result)
FMT = '%H:%M'
imsak = datetime.strptime(jsonData[0]["Imsak"], FMT)
aksam = datetime.strptime(jsonData[0]["Aksam"], FMT)
now = datetime.strptime(now_server.strftime(FMT), FMT)


# Select time gap
if now < imsak:
	print(':watch: ' + str(imsak - now)[:-3])
else:
	print(':watch: ' + str(aksam - now)[:-3])

print("---")
print("Imsak: " + datetime.strftime(imsak, FMT))
print("Aksam: " + datetime.strftime(aksam, FMT))
