#!/usr/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather</bitbar.title>
# <bitbar.version>v3.5.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Detailed weather plugin powered by DarkSky with auto location lookup. Supports metric and imperial units. Needs API key from https://darksky.net/dev/.</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/683200/16276583/ff267f36-387c-11e6-9fd0-fc57b459e967.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>


# -----------------------------------------------------------------------------------
# For a more accurate location lookup, download and install CoreLocationCLI
# Available here: https://github.com/fulldecent/corelocationcli/releases
# This will allow a more percise location lookup as it uses native API for loc lookup
# -----------------------------------------------------------------------------------

import json
import urllib2
import textwrap
from random import randint
import commands

# get yours at https://darksky.net/dev
api_key = ''

# set to si for metric, leave blank for imperial
units = ''

# optional, see message above
core_location_cli_path = '~/CoreLocationCLI'

def mac_location_lookup():
  try:
    exit_code, loc = commands.getstatusoutput(core_location_cli_path + ' -once -format "%latitude,%longitude"')
    if exit_code != 0:
      raise ValueError('CoreLocationCLI not found')
    formatted_city_name = reverse_latlong_lookup(loc)
    return { "loc": loc, "preformatted": formatted_city_name }
  except:
    return False

def auto_loc_lookup():
  try:
    location = urllib2.urlopen('https://ipinfo.io/json')
    return json.load(location)
  except urllib2.URLError:
    return False

def reverse_latlong_lookup(loc):
  try:
    location = json.load(urllib2.urlopen('https://maps.googleapis.com/maps/api/geocode/json?latlng=' + loc  + '&sensor=true'))
    if 'results' in location:
      return location['results'][0]['formatted_address'].encode('UTF-8')
    else:
      return 'Could not lookup location name'
  except:
    return 'Could not lookup location name'

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
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAmVBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAjHWqVAAAAMnRSTlMAAQIDBAUGCAoMFxgZHR4hIiU0NTc4QEFFRlhZZmeGh4qLra6xtL/AwdDR1tnb3Pf4+RkSsW4AAACvSURBVHgBJczZWqJgAADQw7+AouOMU2lGtrgYQhny/g/X9+XtuTgUhGDdZ0IC01JpO1YSCsnrJUabnWz+PiGohwM58m/4jEiW94uP4drWd8coIJuN56bpxoVQBrmiPUN3Av24i9cnVaUZ0uZ5bbXd5Gtzg7Afe3DqoGup8u+zuKW1jCAe/9ftMJz+PCwlxK/vv4TEYZgJTN7msv2jEC8vkgJJNW6V8hSkQO5XQqDwAzg9DY/cb+9eAAAAAElFTkSuQmCC'
  elif icon_code == 'clear-night':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAmVBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAjHWqVAAAAMnRSTlMAAQIDBAUGBxMVFycoKSosMDE/QEpLTFhkZ2prdHZ6ipagoayusLS5u7zGyeXs8vn6+7VdeBQAAACGSURBVHjabU+HDoJQDLxKVRwo4saNgnv1/z/Oiy9PEkOTjrtuVEugqrUSiosDjwnHu3wdekZQP9m1eNjIMbT7d4d+Y32wmBrZEI0mZraCAtTFXUQxtRSCL5GSENy2DrOlawnQswGn+aGvCO1nzJxfe7bL0ebQ8rBJdli2IL/Txbm/5wTV8gEi7AeTMvh8mQAAAABJRU5ErkJggg=='
  elif icon_code == 'rain':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA5klEQVQoFQXBMSuFYRgA0PO870cGlMFgUEpEBiuKgUGMdhmU5AfIbvYHlNGghCIpLBaUWSmDQSbdgdW9j3MChDRkWbdH4+Z8OvGl6AAFu9Kvb+nPq5a0iQoNtqRtRZg3DPakRVSgZR8AVeDKMypMSdNohKqgC6vSLHDgS0UAoOLBPQVLLrR1SQAUXJikwY9BFI3URlEUTGgBG9IMoGoAC9I2BI6lS6fWwJhrL9IRggDrbr1JT26kD4dWQEAIwIpzZ3YARQDQaBQwAbpVoAI6OqDfuz53QhsoAAgM6DGCBAAAAqN6EQD/vPo8tMz6bZYAAAAASUVORK5CYII='
  elif icon_code == 'snow':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA40lEQVQoFQXBsSqFYRgA4Of9vv+4AHWUIhORwYqiMIjZKoMSNyC72Q0oo0EZTh1ZZFGSsigpE0omncKK1/MECGnQsj43xsx6c+Jd8QcU7ErfPqQfj3rSJio02JK2FWHOMNiTFlGBnn0AVIEzt6gwKU2hEaqCFlalGeDAu4oAQMWVSwqWdPxqSQAUdEzQ4EsbRSP9oigKxvWADWkaUDWAeWkbAsdS16k1MOrcnXSEIMC6C0/Sua704tAKCAgBeJau7QCKAKCFfvceAC0VAKBgxBAIAACggAVLCAAACAz49KqNAvwDhLU7zZRMOIUAAAAASUVORK5CYII='
  elif icon_code == 'sleet':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6klEQVQoFQXBry+FYRgA0PO873cVqiCoxgQVG4FwN1E3wWaiZIImEzXJBGN2N6aYovjxBxjdJLuBZO7ncU6AkMZ0DXkwYd67Mx+KP6BgR/r2KQ286EsbqNBgU9pShAXjYFdaQgX69gFQBa49ocK0NINGqAo6WJHmgAMfKgIAFffuKFjW0+pIABT0TNHgyyiKRmpRFAWT+sC6NAuoGsCitAWBU+nKhVUw4dKzdIwgwJpbr9KjGymd6IIIBBKs2DDi3IxtrepPAjSqAHBoHAENYCB0tEL4NawFKCAwak+rNcCbHwAgMOYIBQDgH8ebRS21EV3JAAAAAElFTkSuQmCC'
  elif icon_code == 'wind':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAflBMVEUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACCtoPsAAAAKXRSTlMAAQIDBAUOGx8kJjE7PkVHS05camtvdneHiY+su8DBw8rU1dzm8PH6/PnPEUAAAABvSURBVHgBlc3XDoJgDEDh0+HAvQc4FBHt+7+gI8T8XvJdnKRpk9KSKykB7G/uzjMSRj9ijTRrc2MftweYvxnAJKa9oNPcjw+zYSzOd+pTvtnmxzquI3bPy4BlUVZVWawyEJyEKvaJu4J+y++l0MYLmKUFUAQQSQkAAAAASUVORK5CYII='
  elif icon_code == 'fog':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA7UlEQVQoFQXBzyqEURwA0HPv/SazYGGh7NiwUzOlFHv5s1BWnmAW8wBW3gHlGSyUPIBkQcNkY+ElhEgyDb7v5xyAAiCBpAAJJGHJptqGZR+OHaGogYwD4dnIQN+J8KSLAgXrQg9tQMeDsIJChXN3SEhaWuDGACWDSa9oq2SNxhROLaKmQl+YAwBcG6KQwNCPLXvOdG3bdyWsokBG24Uw9iKEd/fWUAAymDWj6FiQQQEAAAAgkVDUDu36VIQsNCpvdrxKFQK3Rr5lAFljDAkwb1otAULlUSNrIONS+BNCCL9CDxUJMKGtkQAh+dLAP69nSf8xfn8mAAAAAElFTkSuQmCC'
  elif icon_code == 'cloudy':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA70lEQVQoFQXBLUuDURgA0HPvs1cFtVhFHAaTWMVgGgODySKCQQSTSZPFbrEMi8Hgb1gzCDOINruw5EdUGcJg23s9ByAECAFCAECAEGDaFAiAwKaebz/uPfoz0LOBIAsTR56MnDkVBo6dGHt2YCJgVXEOAOBC0QQ63hAqDSE0VDLeXQKfrlEBgAo3vmBXsYIMADIWFftZ26u+UAOAWvjwopWNVAhJApAkgRlDWop1QEiSAKwp2tBVbJs3C2DOgh1FF0juFGMjhzL2DNWKWwkSaGq7Uvz6VnRsWQIpIckmYFlL9qAPQq0kQJYxBgSKGv4BLfNBIGx3s8kAAAAASUVORK5CYII='
  elif icon_code == 'partly-cloudy-day':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAABCUlEQVQoFQXBLUjcYRwA4Od9714F56ZYLAbbmAjC4YIoWIzaLNo8kAmbzW5WYTCDa5v25YnBE0SYTTHIYTEsCSIs+MFO/z+fpwYggaKyb0RLUSFlQBI+mNDBG73oWPZeANTRFCYxZBCfhHEAsoJVDZvuPdjSMI0CFAAbwmcrwiZAlvDWjrYTYQGsuDfk2O+sEuZdW3LgBlu+K0LNs3NndPsjHOoHs376569HXwG+qQyDog6KO6dGfUHhxjq6JZD1YNuREaFJ3ZMBhJBkL/5jDJem3MGaMIMu0IVFYRqQYFf4iD4N9Huyh7qaDPBLaLkV2kLbOyQgySo0zblwbsqVHzqyCoAEACAD8Aph71BRnuBbowAAAABJRU5ErkJggg=='
  elif icon_code == 'partly-cloudy-night':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6UlEQVQoFQXBvyqFYRwA4Od93885AxLDySAMkpiUXAByA7LZbTJYnc4iF+HPxC1YLCYGYVJSslDUCYtTzvD9PE8CAJBl1GoAAKAAKEABAFlt0rY1XZ8SACRZE5tC17uwiwJkGTCkdggOhGUUCmiZsu9VWMW4N/cmkGHGnRB6Tl0IGzp+QYZhPx6t2NACbaHvBA1gT18CNCTsCJfIwJEHNBUJSYVjT0iQ3Zoz4g8JYQDPRhHAkjANGFAw6NsZKqD4cGXIrAXQFl6MIQMs+hJCOHcjdFTIQJLVmtb1zNvSdeAaWQ1AAQBUEgD/3uo/+JyzvikAAAAASUVORK5CYII='
  else:
    icon = ''

  return icon

def get_wx():

  if api_key == "":
    return False

  location = mac_location_lookup() or auto_loc_lookup()

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

    if 'preformatted' in location:
      weather_data['preformatted'] = location['preformatted']

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
    print weather_data['temperature'] + ' | templateImage=' + weather_data['icon']
  else:
    print 'N/A'

  print '---'


  if 'city' in weather_data and 'region' in weather_data:
    print weather_data['city'] + ', ' + weather_data['region'] + ' | href=https://darksky.net/' + weather_data['loc']
  elif 'country' in weather_data:
    print weather_data['country'] + ' | href=https://darksky.net/' + weather_data['loc']
  elif 'preformatted' in weather_data:
    print weather_data['preformatted'] + ' | href=https://darksky.net/' + weather_data['loc']

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
