#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather - OpenWeatherMap</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Grabs simple weather information from openweathermap. Needs configuration for location and API key.</bitbar.desc>
# <bitbar.image>https://daniel.seripap.com/content/images/2016/01/bitbar-openweather.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
from random import randint

location = '5110302'
api_key = '2de143494c0b295cca9337e1e96b00e0'
units = 'imperial' # kelvin, metric, imperial
lang = 'en'

def get_wx():

  if api_key == "":
    return False

  wx = json.load(urllib2.urlopen('http://api.openweathermap.org/data/2.5/weather?id=' + location + '&units=' + units + '&lang=' + lang + '&appid=' + api_key + "&v=" + str(randint(0,100))))

  if units == 'metric':
    unit = 'C'
  elif units == 'imperial':
    unit = 'F'
  else:
    unit = 'K' # Default is kelvin

  try:
    weather_data = {
      'temperature': str(int(round(wx['main']['temp']))),
      'condition': str(wx['weather'][0]['description']),
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
