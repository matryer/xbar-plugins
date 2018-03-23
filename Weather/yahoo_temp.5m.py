#!/usr/bin/env python
# coding=utf-8
#
# <bitbar.title>Yahoo Weather</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>mgjo5899</bitbar.author>
# <bitbar.author.github>mgjo5899</bitbar.author.github>
# <bitbar.desc>It tells you the current weather condition of the location where your computer is located at.  It knows the location of the computer by using its public IP.  You can also manually set the city and region through modifying the file. </bitbar.desc>
# <bitbar.image>https://i.imgur.com/YNypf0P.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by mgjo5899

import urllib
import json

ip_url = 'http://ip-api.com/json'

try:
  r = urllib.urlopen(ip_url).read()
except IOError:
  print("Server loading...")
  exit(1)

j = json.loads(r)
city = str(j['city'])
region = str(j['region'])

####### IF YOU WANT TO MANUALLY CHOOSE THE LOCATION #########
#############################################################
# 1. Set the city and region as needed
#city = 'quebec'
#region = 'canada'

url = 'https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22' + city + '%2C%20' + region + '%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys'

try:
  r = urllib.urlopen(url).read()
except IOError:
  print("Server loading...")
  exit(1)

j = json.loads(r)

# change unit to 'c' if you want celsius
# change unit to 'f' if you want fahrenheit
unit = 'c'

try:
  cond = j['query']['results']['channel']['item']['condition']['text']
  temp = float(j['query']['results']['channel']['item']['condition']['temp'])
except TypeError:
  print("Server Loading...")
  exit(1)

if unit == 'c':
  temp = (temp  - 32.0) * 5.0 / 9.0
  print(str(cond) + ' : ' + str(int(temp)) + '°C')
elif unit == 'f':
  print(str(cond) + ' : ' + str(int(temp)) + '°F')
