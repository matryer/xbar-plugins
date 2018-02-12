#!/usr/bin/env python
# coding=utf-8
#
# <bitbar.title>Yahoo Weather</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>mgjo5899</bitbar.author>
# <bitbar.author.github>mgjo5899</bitbar.author.github>
# <bitbar.desc>It tells you current condition of your chosen city.  Very simple app with simple configuration.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/YNypf0P.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by mgjo5899

import subprocess
import json

# 1. Go to Yahoo's developer website for weather
#     - https://developer.yahoo.com/weather/#curl
# 2. Click the Forecast for Nome, AK example
# 3. Change the end part of YQL Query to your chosen city, state
#     - For Champaign, IL: select * from weather.forecast where woeid in (select woeid from geo.places(1) where text="champaign, il")
#     - For Seoul, South Korea: select * from weather.forecast where woeid in (select woeid from geo.places(1) where text="seoul, kr")
# 4. Copy the Endpoint from the website, below the response, and paste it here 
url = 'https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20weather.forecast%20where%20woeid%20in%20(select%20woeid%20from%20geo.places(1)%20where%20text%3D%22champaign%2C%20il%22)&format=json&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys'

r = subprocess.check_output(['curl', url], stderr=subprocess.STDOUT)
s = r.find('query') - 2
j = json.loads(r[s:])

# Change unit to 'C' if you want Celsius
# Change unit to 'F' if you want Fahrenheit
unit = 'C'

cond = j['query']['results']['channel']['item']['condition']['text']
temp = float(j['query']['results']['channel']['item']['condition']['temp'])

if unit == 'C':
  temp = (temp  - 32.0) * 5.0 / 9.0
  print(str(cond) + ' : ' + str(int(temp)) + '°C')
elif unit == 'F':
  print(str(cond) + ' : ' + str(int(temp)) + '°F')
