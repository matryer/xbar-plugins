#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather</bitbar.title>
# <bitbar.version>v3.1.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Detailed weather plugin powered by DarkSky with auto location lookup. Supports metric and imperial units. Needs API key from https://darksky.net/dev/.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/683200/16276583/ff267f36-387c-11e6-9fd0-fc57b459e967.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
import textwrap
from random import randint

api_key = '' # get yours at https://darksky.net/dev
units = '' # set to si for metric, leave blank for imperial

def auto_loc_lookup():
  try:
    location = urllib2.urlopen('http://ipinfo.io/json')
    return json.load(location)
  except urllib2.URLError:
    return False

def full_country_name(country):
  try:
    countries = json.load(urllib2.urlopen('http://country.io/names.json'))
    try:
      if country in countries:
        return countries[country].encode('UTF-8')
      else:
        return False
    except KeyError:
      return False
  except urllib2.URLError:
    return False

def calculate_bearing(degree):
  cardinals = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']
  return cardinals[int(round(((6 * degree)) / 360))]

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

  for locData in location:
    locData.encode('utf-8')

  try:
    if 'loc' in location:
      wx = json.load(urllib2.urlopen('https://api.darksky.net/forecast/' + api_key + '/' + location['loc'] + '?units=' + units + "&v=" + str(randint(0,100))))
    else:
      return False
  except urllib2.HTTPError:
    return False

  if units == 'si':
    unit = 'C'
    distance = 'm/s'
    distance_short = 'km'
  else:
    unit = 'F'
    distance = 'mph'
    distance_short = 'mi'

  try:

    weather_data = {}

    if 'currently' in wx:
      for item in wx['currently']:
        if item == 'temperature':
          weather_data['temperature'] = str(int(round(wx['currently']['temperature']))) + '°' + unit
        elif item == 'icon':
          weather_data['icon'] = get_wx_icon(str(wx['currently']['icon']))
        elif item == 'summary':
          weather_data['condition'] = str(wx['currently']['summary'].encode('utf-8'))
        elif item == 'windSpeed':
          weather_data['wind'] = str(wx['currently']['windSpeed']) + ' ' + distance
        elif item == 'windBearing':
          weather_data['windBearing'] = calculate_bearing(wx['currently']['windBearing'])
        elif item == 'humidity':
          weather_data['humidity'] = str(int(round(wx['currently']['humidity'] * 100))) + '%'
        elif item == 'dewPoint':
          weather_data['dewPoint'] = str(wx['currently']['dewPoint'])
        elif item == 'visibility':
          weather_data['visibility'] = str(int(round(wx['currently']['visibility']))) + ' ' + distance_short
        elif item == 'pressure':
          weather_data['pressure'] = str(wx['currently']['pressure']) + ' mb'
        elif item == 'apparentTemperature':
          weather_data['feels_like'] = str(int(round(wx['currently']['apparentTemperature']))) + '°' + unit

    if 'minutely' in wx:
      for item in wx['minutely']:
        if item == 'summary':
          weather_data['next_hour'] = str((wx['minutely']['summary'].encode('utf-8')))

    if 'daily' in wx:
      for item in wx['daily']:
        if item == 'summary':
          weather_data['week'] = str((wx['daily']['summary'].encode('utf-8', 'ignore')))

    if 'city' in location and 'region' in location:
      if location['city'] == '' and location['region'] == '':
        if 'country' in location:
            country = full_country_name(location['country'])

            if country is False or location['country'] == '':
              weather_data['country'] = 'See Full Forecast'
            else:
              weather_data['country'] = country
      else:
        weather_data['city'] = str(location['city'].encode('utf-8'))
        weather_data['region'] = str(location['region'].encode('utf-8'))

    if 'loc' in location:
      weather_data['loc'] = str(location['loc'])

  except KeyError:
    return False

  return weather_data

def render_wx():

  if api_key == '':
    print 'Missing API key'
    print '---'
    print 'Get an API Key | href=https://darksky.net/dev'
    return False

  weather_data = get_wx()

  if weather_data is False:
    print '--'
    print '---'
    print 'Could not get weather data at this time'
    return False

  if 'icon' in weather_data and 'temperature' in weather_data:
    print weather_data['icon'] + ' ' + weather_data['temperature']
  else:
    print 'N/A'

  print '---'

  if 'city' in weather_data and 'region' in weather_data:
    print weather_data['city'] + ', ' + weather_data['region'] + ' | href=https://darksky.net/' + weather_data['loc']
  elif 'country' in weather_data:
    print weather_data['country'] + ' | href=https://darksky.net/' + weather_data['loc']

  if 'condition' in weather_data and 'feels_like' in weather_data:
    print weather_data['condition'] + ', Feels Like: ' + weather_data['feels_like']

  print '---'

  if 'next_hour' in weather_data:
    print weather_data['next_hour']
    print '---'

  print '---'

  if 'week' in weather_data:
    print "\n".join(textwrap.wrap(weather_data['week'], 50))
    print '---'

  if 'wind' in weather_data and 'windBearing' in weather_data:
    print 'Wind: ' + weather_data['wind'] + ' ' + weather_data['windBearing']

  if 'humidity' in weather_data:
    print 'Humidity: ' + weather_data['humidity']

  if 'dewPoint' in weather_data:
    print 'Dew Point: ' + weather_data['dewPoint']

  if 'visibility' in weather_data:
    print 'Visibility: ' + weather_data['visibility']

  if 'pressure' in weather_data:
    print 'Pressure: ' + weather_data['pressure']

  print '---'
  print 'Powered by DarkSky | href=https://darksky.net/poweredby/?ref=bitbarWeather'

render_wx()
