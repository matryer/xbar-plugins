#!/usr/bin/env /anaconda3/bin/python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather - OpenWeatherMap</bitbar.title>
# <bitbar.version>v1.0.2</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Grabs simple weather information from openweathermap. Needs configuration for location and API key.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib.request, urllib.error
import emoji
from random import randint

location = '5102922'
api_key = '4bccba5d61d516608bd9d2adf5bf04af'
units = 'imperial' # kelvin, metric, imperial
lang = 'en'

def get_wx():

  if api_key == "your api key goes here!":
    return False

  try:
    wx = json.load(urllib.request.urlopen('http://api.openweathermap.org/data/2.5/weather?id=' + location + '&units=' + units + '&lang=' + lang + '&appid=' + api_key + "&v=" + str(randint(0,100))))
  except urllib.error.URLError:
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
      'condition': wx['weather'][0]['description'],
      'id': wx['weather'][0]['id'],
      'city': wx['name'],
      'unit': '°' + unit
    }
  except KeyError:
    return False

  return weather_data

def render_wx():
  weather_data = get_wx()
  emoji_dict = {
  200 : ":zap:",  201 : ":zap:",  202 : ":zap:",  210 : ":zap:",  211 : ":zap:",  212 : ":zap:",  221 : ":zap:",  230 : ":zap:",  231 : ":zap:",  232 : ":zap:",
  300 : ":umbrella:",  301 : ":umbrella:",  302 : ":umbrella:",  310 : ":umbrella:",  311 : ":umbrella:",  312 : ":umbrella:",  313 : ":umbrella:",  314 : ":umbrella:",  321 : ":umbrella:",  
  500 : ":umbrella:",  501 : ":umbrella:",  502 : ":umbrella:",  503 : ":umbrella:",  504 : ":umbrella:",  511 : ":umbrella:",  520 : ":umbrella:",  521 : ":umbrella:",  522 : ":umbrella:",  531 : ":umbrella:",
  600 : ":snowflake:",  601 : ":snowflake:",  602 : ":snowflake:",  611 : ":snowflake:",  612 : ":snowflake:",  613 : ":snowflake:",  615 : ":snowflake:",  616 : ":snowflake:",  620 : ":snowflake:",  621 : ":snowflake:",  622 : ":snowflake:",
  701 : ":fog:",  711 : ":fog:",  721 : ":fog:",  731 : ":fog:",  741 : ":fog:",  751 : ":fog:",  761 : ":fog:",  762 : ":fog:",  771 : ":fog:",
  781 : ":cyclone:",
  800 : ":sunny:",
  801 : ":partly_sunny:",  802 : ":partly_sunny:",  803 : ":cloud:",  804 : ":cloud:",
  }
  tridash = '\n' + '---' + '\n'

  if weather_data is False:
    return 'Err' + tridash + 'Could not get weather; Maybe check API key or location?'
  
  emojiweather = emoji.emojize(emoji_dict[weather_data['id']])

  #weather_data['condition'] + ' ' + 
  emoji_t = '' + emojiweather + weather_data['temperature'] + weather_data['unit'] 
  condi = [x.capitalize() for x in  weather_data['condition'].split(' ')]
  return emoji_t + tridash + ' '.join(condi) + ' | refresh = true'

print(render_wx())