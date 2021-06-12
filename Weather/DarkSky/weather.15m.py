#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <xbar.title>Weather</xbar.title>
# <xbar.version>v3.5.0</xbar.version>
# <xbar.author>Daniel Seripap, Valerio</xbar.author>
# <xbar.author.github>seripap, scartiloffista</xbar.author.github>
# <xbar.desc>Detailed weather plugin powered by DarkSky with auto location lookup. Supports metric and imperial units. Needs API key from https://darksky.net/dev/.</xbar.desc>
# <xbar.image>https://cloud.githubusercontent.com/assets/683200/16276583/ff267f36-387c-11e6-9fd0-fc57b459e967.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>


# -----------------------------------------------------------------------------------
# For a more accurate location lookup, download and install CoreLocationCLI
# Available here: https://github.com/fulldecent/corelocationcli/releases
# This will allow a more precise location lookup as it uses native API for loc lookup
# -----------------------------------------------------------------------------------

import json
import urllib.request, urllib.error, urllib.parse
import textwrap
from random import randint
import subprocess

# get yours at https://darksky.net/dev
api_key = ''

# get yours API key for encode location at https://opencagedata.com
geo_api_key = ''

# if you want to set manual location, define following two vars. If left empty, script will try to determine the location
# example:
# manual_city = 'Novi Sad'
# manual_latlng = '45.2526331,19.7817785'
manual_city = ''
manual_latlng = ''


# set to si for metric, leave blank for imperial
units = 'si'

# optional, see message above
core_location_cli_path = '~/CoreLocationCLI'

def manual_location_lookup():
  if manual_latlng == "" or manual_city == "":
     return False;
  else:
     return { "loc": manual_latlng, "preformatted": manual_city }

def mac_location_lookup():
  try:
    exit_code, loc = subprocess.getstatusoutput(core_location_cli_path + ' -once -format "%latitude,%longitude"')
    if exit_code != 0:
      raise ValueError('CoreLocationCLI not found')
    formatted_city_name = reverse_latlong_lookup(loc)
    return { "loc": loc, "preformatted": formatted_city_name }
  except:
    return False

def auto_loc_lookup():
  try:
    location = urllib.request.urlopen('https://ipinfo.io/json')
    return json.load(location)
  except urllib.error.URLError:
    return False

def reverse_latlong_lookup(loc):
  try:
    location_url = 'https://api.opencagedata.com/geocode/v1/json?q=' + loc + '&key=' + geo_api_key + '&language=en&pretty=1'
    location = json.load(urllib.request.urlopen(location_url))
    if 'results' in location:
      return location['results'][0]['formatted']
    else:
      return 'Could not lookup location name'
  except:
    return 'Could not lookup location name'

def full_country_name(country):
  try:
    countries = json.load(urllib.request.urlopen('http://country.io/names.json'))
    try:
      if country in countries:
        return countries[country]
      else:
        return False
    except KeyError:
      return False
  except urllib.error.URLError:
    return False

def calculate_bearing(degree):
  cardinals = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW']
  return cardinals[int(round(((6 * degree)) / 360))]

def get_wx_icon(icon_code):
  if icon_code == 'clear-day':
    icon = ":sunny:"
  elif icon_code == 'clear-night':
    icon = ":crescent_moon:"
  elif icon_code == 'rain':
    icon = ":cloud_with_rain:"
  elif icon_code == 'snow':
    icon = ":snowflake:"
  elif icon_code == 'sleet':
    icon = ":cloud_with_snow:"
  elif icon_code == 'wind':
    icon = ":dash:"
  elif icon_code == 'fog':
    icon = ":fog:"
  elif icon_code == 'cloudy':
    icon = ":cloud:"
  elif icon_code == 'partly-cloudy-day':
    icon = ":sun_behind_small_cloud:"
  elif icon_code == 'partly-cloudy-night':
    icon = ":sun_behind_large_cloud:"
  else:
    icon = ''

  return icon

def get_wx():

  if api_key == "":
    return False

  location = manual_location_lookup() or mac_location_lookup() or auto_loc_lookup()

  if location is False:
    return False

  for locData in location:
    locData

  try:
    if 'loc' in location:
      wx = json.load(urllib.request.urlopen('https://api.darksky.net/forecast/' + api_key + '/' + location['loc'] + '?units=' + units + "&v=" + str(randint(0,100))))
    else:
      return False
  except urllib.error.HTTPError:
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
          weather_data['condition'] = str(wx['currently']['summary'])
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
          weather_data['next_hour'] = str((wx['minutely']['summary']))

    if 'daily' in wx:
      for item in wx['daily']:
        if item == 'summary':
          weather_data['week'] = str((wx['daily']['summary']))

    if 'city' in location and 'region' in location:
      if location['city'] == '' and location['region'] == '':
        if 'country' in location:
            country = full_country_name(location['country'])

            if country is False or location['country'] == '':
              weather_data['country'] = 'See Full Forecast'
            else:
              weather_data['country'] = country
      else:
        weather_data['city'] = str(location['city'])
        weather_data['region'] = str(location['region'])

    if 'loc' in location:
      weather_data['loc'] = str(location['loc'])

    if 'preformatted' in location:
      weather_data['preformatted'] = location['preformatted']

  except KeyError:
    return False

  return weather_data

def render_wx():

  if api_key == '':
    print('Missing API key')
    print('---')
    print('Get an API Key | href=https://darksky.net/dev')
    return False

  weather_data = get_wx()

  if weather_data is False:
    print('--')
    print('---')
    print('Could not get weather data at this time')
    return False

  if 'icon' in weather_data and 'temperature' in weather_data:
    print(weather_data['icon'] + " " + weather_data['temperature'] + "| emojize=true symbolize=false")
  else:
    print('N/A')

  print('---')


  if 'city' in weather_data and 'region' in weather_data:
    print(weather_data['city'] + ', ' + weather_data['region'] + ' | href=https://darksky.net/' + weather_data['loc'])
  elif 'country' in weather_data:
    print(weather_data['country'] + ' | href=https://darksky.net/' + weather_data['loc'])
  elif 'preformatted' in weather_data:
    print(weather_data['preformatted'] + ' | href=https://darksky.net/' + weather_data['loc'])

  if 'condition' in weather_data and 'feels_like' in weather_data:
    print(weather_data['condition'] + ', Feels Like: ' + weather_data['feels_like'])

  print('---')

  if 'next_hour' in weather_data:
    print(weather_data['next_hour'])
    print('---')

  print('---')

  if 'week' in weather_data:
    print("\n".join(textwrap.wrap(weather_data['week'], 50)))
    print('---')

  if 'wind' in weather_data and 'windBearing' in weather_data:
    print('Wind: ' + weather_data['wind'] + ' ' + weather_data['windBearing'])

  if 'humidity' in weather_data:
    print('Humidity: ' + weather_data['humidity'])

  if 'dewPoint' in weather_data:
    print('Dew Point: ' + weather_data['dewPoint'])

  if 'visibility' in weather_data:
    print('Visibility: ' + weather_data['visibility'])

  if 'pressure' in weather_data:
    print('Pressure: ' + weather_data['pressure'])

  print('---')
  print('Powered by DarkSky | href=https://darksky.net/poweredby/?ref=xbarWeather')

render_wx()
