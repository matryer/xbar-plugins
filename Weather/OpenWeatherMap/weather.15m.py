#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather - OpenWeatherMap</bitbar.title>
# <bitbar.version>v1.0.2</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Grabs simple weather information from openweathermap. Needs configuration for location and API key.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
from random import randint

location = '5110302'
api_key = '8b4824b451d5db1612156837df880f55'
units = 'imperial' # kelvin, metric, imperial
lang = 'en'

def get_wx():

  if api_key == "":
    return False

  try:
    wx = json.load(urllib2.urlopen('http://api.openweathermap.org/data/2.5/weather?id=' + location + '&units=' + units + '&lang=' + lang + '&appid=' + api_key + "&v=" + str(randint(0,100))))
  except urllib2.URLError:
    return False

  if units == 'metric':
    unit = 'C'
  elif units == 'imperial':
    unit = 'F'
  else:
    unit = 'K' # Default is kelvin

  try:
    weather_data = {
      'temperature': str(int(round(wx['main']['temp']))),
      'condition': str(wx['weather'][0]['description'].encode('utf-8')),
      'city': wx['name'],
      'unit': '°' + unit
    }
  except KeyError:
    return False

  return weather_data

def render_wx():
  weather_data = get_wx()

  if weather_data is False:
    return 'Could not get weather'

  return weather_data['condition'] + ' ' + weather_data['temperature'] + weather_data['unit']

print render_wx()
