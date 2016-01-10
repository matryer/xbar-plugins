#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather</bitbar.title>
# <bitbar.version>v1.3.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Detailed weather plugin powered by forecast.io. Auto location lookup, needs API key from http://developer.forecast.io.</bitbar.desc>
# <bitbar.image>https://daniel.seripap.com/content/images/2016/01/weather.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
from random import randint

api_key = '' # get yours at https://developer.forecast.io
units = '' # change to si for metric, default is imperial

def auto_loc_lookup():
  try:
    return json.load(urllib2.urlopen('http://ipinfo.io/json'))
  except urllib2.HTTPError:
    return False

def calculate_bearing(degree):
  cardinals = ['N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N']
  return cardinals[int(round(((degree + 11.25) % 360) / 22.5))]

def get_wx_icon(icon_code):
  if icon_code == 'clear-day':
    icon = '☀️'
  elif icon_code == 'clear-night':
    icon = '☀'
  elif icon_code == 'rain':
    icon = '🌧'
  elif icon_code == 'snow':
    icon = '🌨'
  elif icon_code == 'sleet':
    icon = '🌨'
  elif icon_code == 'wind':
    icon = '💨'
  elif icon_code == 'fog':
    icon = '(FOG)'
  elif icon_code == 'cloudy':
    icon = '☁'
  elif icon_code == 'partly-cloudy-day':
    icon = '⛅'
  elif icon_code == 'partly-cloudy-night':
    icon = '⛅'
  else:
    icon = ''

  return icon

def get_wx():

  if api_key == "":
    return False

  location = auto_loc_lookup()

  if location is False:
    return False

  try:
    wx = json.load(urllib2.urlopen('https://api.forecast.io/forecast/' + api_key + '/' + location['loc'] + '?units=' + units + "&v=" + str(randint(0,100))))
  except urllib2.HTTPError:
    return False

  if units == 'si':
    unit = 'C'
    distance = 'km/h'
    distance_short = 'km'
  else:
    unit = 'F'
    distance = 'mph'
    distance_short = 'mi'

  try:
    weather_data = {
      'temperature': str(int(round(wx['currently']['temperature']))) + '°' + unit,
      'icon': get_wx_icon(str(wx['currently']['icon'])),
      'condition': str(wx['currently']['summary']),
      'wind': str(wx['currently']['windSpeed']) + ' ' + distance,
      'windBearing': calculate_bearing(wx['currently']['windBearing']),
      'humidity': str(int(round(wx['currently']['humidity'] * 100))) + '%',
      'dewPoint': str(wx['currently']['dewPoint']),
      'visibility': str(int(round(wx['currently']['visibility']))) + ' ' + distance_short,
      'pressure': str(wx['currently']['pressure']) + ' mb',
      'feels_like': str(int(round(wx['currently']['apparentTemperature']))) + '°' + unit,
      #'next_hour': str(wx['minutely']['summary']), #Forecast.io doesn't have this field, so this is throwing an error.
      'next_twentyfour_hours': str(wx['hourly']['summary']),
      'city': str(location['city']),
      'region': str(location['region'])
    }
  except KeyError:
    return False

  return weather_data

def render_wx():
  weather_data = get_wx()

  if weather_data is False:
    print 'Could not get weather'
    return False

  print weather_data['icon'] + ' ' + weather_data['temperature']
  print '---'
  print weather_data['city'] + ', ' + weather_data['region']
  print weather_data['condition'] + ', Feels Like: ' + weather_data['feels_like']
  print 'Wind: ' + weather_data['wind'] + ' ' + weather_data['windBearing']
  print 'Humidity: ' + weather_data['humidity']
  print 'Dew Point: ' + weather_data['dewPoint']
  print 'Visibility: ' + weather_data['visibility']
  print 'Pressure: ' + weather_data['pressure']

render_wx()
