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

# get yours API key for encode location at https://opencagedata.com
geo_api_key = ''

# if you want to set manual location, define following two vars. If left empty, script will try to determine the location
# example:
# manual_city = 'Novi Sad'
# manual_latlng = '45.2526331,19.7817785'
manual_city = ''
manual_latlng = ''


# set to si for metric, leave blank for imperial
units = ''

# optional, see message above
core_location_cli_path = '~/CoreLocationCLI'

def manual_location_lookup():
  if manual_latlng == "" or manual_city == "":
     return False;
  else:
     return { "loc": manual_latlng, "preformatted": manual_city }

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
    location_url = 'https://api.opencagedata.com/geocode/v1/json?q=' + loc + '&key=' + geo_api_key + '&language=en&pretty=1'
    location = json.load(urllib2.urlopen(location_url))
    if 'results' in location:
      return location['results'][0]['formatted'].encode('UTF-8')
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
    icon = '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'
  elif icon_code == 'clear-night':
    icon = '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'
  elif icon_code == 'rain':
    icon = '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'
  elif icon_code == 'snow':
    icon = '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'
  elif icon_code == 'sleet':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAAFAAAABaCAQAAACFgxlAAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAARt4AAEbeAY4mMlsAAAAHdElNRQfkAwQTLi3Ze1bDAAAAEGNhTnYAAACCAAAAgwAAABsAAAAVtjXJAgAAB3ZJREFUaN7t23twVdUVx/FPHoRAEHmjvKSIohGx8hBaFK0o1dGxFu3UR7VVWm3H2plWZ7RlRmnHTuvUAR+1dUYHXxWVGW0ZtQWKFaEoBWuxVawwQCggECEgCXmT1T9yuSZIIDe5yfEPf/kj95yzz7rf7HP23mutvZIjO+pmhJONdrohjtFDoRqVypVY633v2qSqbYZzsoBWbIrznaGv7odtUa3Uv63wN++o6VzAXqa6ztn6plF222W//aoUKlJkgN66pa7uttx8i+3uHMAi033XRF1RZ7OVVvlAiTLV6jTIla+bvoYa5cvGG64Adf7hd15S0a6OaYUmeUG1EEo97UrD5B2hdZ4hpptnpxCqvWxyR8L1cKftQvjQ/SbIb+V9XYw1247Unbfp2TF4wzylXqg234SMX5EcE81TJdR7zrDs4431dyGsd30LY/boKnSjjUJ4zZhs4/1LCK8a125Li4WwxoTs4Z3pbaHB447PgrVBnhXCW0YfqVlea+0Zbq4Jwly32ZUFwHKv6etMg4xRZaIpTjJArQrRFnM9PC2Ex/XOAtxB9facEA4IoUGl/3rMua2eF9LKcac6YbGBWcSDIZYIoVKFWiGE3R42NDMz5/lIWOfMLOPB6X7je6aa4jJ3WKRcCEszGd+9LBSqfKsD8A5Vd5daJoQ3ndjam2aoE+alF/2O1mB/EMLzerSm+QCrhJ0mdhIe9PeyUOPa1jT+jnphttxOBGScrcLraUeuRRVZLPxPcafiwRzhYxcfrdlke4XfZ8HzzlRTVQizW5oSBxpltMHOdqxKL7Ztdm+XPvChkxR/GjDfWaa70Ij0GHrXPzsdj33KMPBQwNFudaU+oNZelWo9rywBwAb16NIUMN9V7jYSVd62xFtKlKvycQJ4FOlN067paqYKoc4il+uVCFRTfcle4ZGDh/nuViOUut2xSbOBnwuVrj54OMN+ocTXkuZKaZR1wmqDGg/H2izscGnSXCnlmC2EexsPCz0r1Lglaa4mut4+YVmj/zlNufCCoqSpmijP/UKNGeR7QtjjvKSZDtFYpcJ8TrFR+JPCpIkOUS+rhTW5JhuMhaqTJjpE9WqRn2ucAmVWJ83zKfUxENtznYodtiXN8ymd7wSs4D/C8s/UCIZi7wibjWW9sETXpInQPRWwF7rASiHMkpOvFgXyM88eZ1mF7jPAepxusl6Y70HBUuG9rGcMMtcZSlOZhRDKPWwA5FvnXMcZbGfCgBfrL+xTo8xb5nlVbeOFGWqFmxPG62el8L6pih3fPOdWbJOwIOGV5Dq1wi8Pd6lxLd7rKwni9bVM2OaMw19u9Gb+mOBc+EP1wpyWou9C84RatycQnsNp1gkbjpS9GKtEKPONBPCKzBMO+MmRm92gQtjqmk5OEuW4Q53wytFSy/lmqhH2mnmEpjmpkD57ukqZsMHYozft6qfKhXpLXHEYyHyn+ZU3XJJFvEttE/b4Zuua57nGOiFUecM9LjPGCYY52TluMs8WIWx1RZbwptsiVLktk8F5qkd8lFoPa3xks40+TCW2wwEHhFI3KWgnXIHvKxWq3ZWprXwT3WeNfU2W7lBnm5fcaJZyodKD7dptGuQBlUKFnx3N0WupcwcYpdhwveSptMN6a21SLc8N7jEQK/zaInVt6LtpZpqEHe4y14F2/KEtaKo3hbDPoyZksJlGvkmeTD2blc7PPtpBDfWQvULY6TEXtCoX1ttXPZl6v/eYk+leUqbKN81CNSnncoW7XWjIYd+nQkNM8wtv2J8qCHjFtNb3e3tW354u8m1TUqniKlutV6JEmSo1uuhmgKFGOtGg1Ob3Pss96S+ZlFS01z0oMsklpjqp2T5UgwY5zXppvw2WeMkqlZl9QXb8l4HGGG+8Efo7Vhe5coUDapUrtd4aK73XtqAimw5WFz310V9P3RWoU61MmV0+Vp/Fb/lcn6uT1RFRSJ5BClMFUG2sGuxY9bLIdtusc1b7jWVcANIK5RroOFRlI2eWrRCp0I9dlPrcoKHZb0aa1RGFZK1XV7PUKjENecalA4frFeILFgsLDE4Kr4u7Un7NBjd71JZUJVEo82czLEwdLUgqyVdgjoYURH2zQOGTsqcQXu9oH7BlHeO3h0Fr/rPMqKTwGhGfOSLe2rbWjWRrmqltZmmT1aqM9sW0T9jlYL40KY23K91bLzpFLvqbpTJ99kfJAv4gDfKeEemzBZ5Kn3++bU8rWxP1J1B/tTH9udaL6ch5WNuK07K3khxU80L4svS7V5BsD36CVdwM5NR0MfO+tm0VZQtwbTrumNakIGyEGWmHbm2m8Vx2NTy1/oZQ4lbFRvp6qpayMVy/PEk8uLfZ8rbdFlVNzixva6lQth7xYKc1s3qcIc22hga1tAfSORpiwVFW4vCBc5PC69ZkOt6uohlWva1pN+JdIzM3no1HXGOpPWCVy13tmVRyqMFrbnGhJwQaLFWaVB9ygz1Wp4qz+3lHCPtTtTi9zVXrIcckh0euy9LDoE/qnzoqnJM608+1n4GCv7T6WJPqwXPab6wjws4G2/XToDIbdRD/B8A26aoIG6gOAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTAzLTA0VDE5OjQ2OjMyKzAwOjAwxbZSTQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0wMy0wNFQxOTo0NjozMiswMDowMLTr6vEAAAAZdEVYdFNvZnR3YXJlAHd3dy5pbmtzY2FwZS5vcmeb7jwaAAAAAElFTkSuQmCC'
  elif icon_code == 'wind':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAAFoAAAA0CAQAAACgX+ejAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAARt4AAEbeAY4mMlsAAAAHdElNRQfkAwQTLi3Ze1bDAAAAEGNhTnYAAADDAAAAxAAAADUAAABIB/ZTzgAAA85JREFUaN7tmU1oVEccwH9vd3aT0GRj4ldKpUZCqiERJApCcrAN9aPoQcSD4MEPFAyouamgUCkeEgpFES/SHlqqpUWw7aEUIcXUb9EIHoyCkgiN2RiNkl3XfOzu9GCjMZm8mbeZl6fQ3zvlvf/834/JvPnPzDrYJ0YdX7CYWaSJc5lztJP24T3WcGjgLM+Rr68M3RxnUdBikxNlL71jhN9ct1kVtJyaEE2klMoSSRefBS2oYhV9kypLJNeZH7TieIr5w1VZIvnSzqsc5hL1FD9IH1LxZA1n+EDT+g6r+Wfq0oKjLCVrHB/mGjsYVDz5VKsMFdTakS6n0lOLXkKKuxFqDNrmUcXvxm8qpowYkgHiDLwtbYcCSo3i5hjmq2EjDZRTCCTopJUzdLyR7qLUw/AQdCmjHRyj9tIgpohdNLLg9d8lfMwKtnCCk7x4JdHk+UMcUtxP0WfUPq6NmEkzWxUjoIIWKjnIMw+2Wg5rJzzJC9ZosuRz1KV9lhZPXaxlGT1a6VuUabJscqmpEkmC9SCYY8m9nxusc42QnNIMj1IaKXCNKKSRNsExaj18iG7M0jw/z4+aiGUs1b5lOcsF5XxiRVnHHfZpP8N6gwJVTEPIUi/ruMh2bmhiwmOmOTcqBWGfdUfo5DTfGZRvQYlRxhmCX7iJIOKDbj5p4rRzlS6j+KxyTTORpOAbHON65o0Q0tPecIROo7hOAUij4uqdjOcWl3mpmfIgRZsvtjlTxF/aAnWFDwV1lJIhi+NTf5vjkNGO/mG+pcehjXrSgQuPEtLU5x/YTUIQIez7tGeLXzlEAsQ708c6+vmeZh6DvZ2Lf2RJEucCp7gwOoE6rGXuO93bSeI8pDuHCfR/poxD1JcS7o5keGrSzVQhyeJMW3lxGKSbB1yhw3CJNIGrBhtSP64svfxEfW7/50sBSb+6ethD3vvT06PXSw54rciCDvKQ/y2Z/ECSzyKXFUU++7nHWS8pHUqIAFnfPsIM6zipWSVfZIPhCdU0EeW0dogMs9lLSkGUIkt6GQYUe/uFBr+1RFjJz+ZbM0EdX1s5YwrRQSNPJ9yv0h7iACymkOfm0sUssbTWk8o8ZUbZY8TMpUOYnRmbSavvmuT3dB4gSPHQyvAI80i5fOwlY9DXibd/oNBJX+JzSzP0sPLA+z7PmK1te5ekFQdLFNKqnfIy7AxaczzbGNJItzMvaMnxzOA3V+UUW4NWVFHNzUmV07Tkss6bDmo5r1Qe4AixoOUmZx5fcZ+RMcJJWtmQyyHzdO4PHapZyQo+ooB+7vInf/Mkl0T/Ao94doovE2BiAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIwLTAzLTA0VDE5OjQ2OjMyKzAwOjAwxbZSTQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMC0wMy0wNFQxOTo0NjozMiswMDowMLTr6vEAAAAZdEVYdFNvZnR3YXJlAHd3dy5pbmtzY2FwZS5vcmeb7jwaAAAAAElFTkSuQmCC'
  elif icon_code == 'fog':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAAFoAAAAeCAQAAAAGaMHzAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAAmJLR0QA/4ePzL8AAAAJcEhZcwAARt4AAEbeAY4mMlsAAAAHdElNRQfkAwQTLi3Ze1bDAAAAEGNhTnYAAADVAAAA1QAAAD4AAABcxaWZlwAAAKBJREFUWMPt2MERwjAMRNG/jiF10A0lUQD1UAlHDimFAE44UIEuFjvjV8GORiPZEndmdnyIVTyZs3MErcWqyj97QdkZwiQu1OwUQZ/sAMNfE1cOVhNEvMXKMTtH0KuwZWcI2wpTdoawqXLzaw8x2+1Ep7FhTpz82kMsfu/psVw62SqNlp0iqIkzk9Xkk12Rh74sP7bjWNOJ57Gm8vA7QH4B1oUmCMzIWB0AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMDMtMDRUMTk6NDY6MzIrMDA6MDDFtlJNAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTAzLTA0VDE5OjQ2OjMyKzAwOjAwtOvq8QAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAAASUVORK5CYII='
  elif icon_code == 'cloudy':
    icon = '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'
  elif icon_code == 'partly-cloudy-day':
    icon = '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'
  elif icon_code == 'partly-cloudy-night':
    icon = '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'
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
