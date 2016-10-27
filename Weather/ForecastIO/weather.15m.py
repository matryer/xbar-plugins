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
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSIbPCQ0OgAAAYZJREFUOMudlbFKA0EQhr/IkT4KitgFEVNcIIaojUGx1O6KoIhC8hAKvoZlUlim0wewSApbWxUb7dRISJ/zbP6D5czu3WVgOPZm5t/ZmX92IbvsAT+An8V5IQfwPrAILM0DvArULL4r+oYWe8G1UR+IgM4M2ynwDJRn2KrAPVCyAVcFHAGVjCXaNWL8NMdvoKV1EQiAK2mgfwAe8CHQdpYsPNWsAUxU1zirUP/q8mlnZUosDQFNgR5wIe0Bv7LVySlFZTW1sGRL4BOjLP/EBw40AHHHAx2569i8J5/AVsuBQZMXYBPYEMeHDuCh6ruu9RmwA3wCAw84Bg41AI95SJ+QayUFMPYElgR8VQ2bwK0FqCmfN62PgDVVYZTWvFCNmtW8MK15Nqkn6HYu7Rq8zkU3X00pKDBtQLwsoB0FvxsB8UhfSs2RPgG+dA04M42z2s54uooRU7U5lYA7i0NZV2bLccq+K4OC41mKgBuLvQYsJyfPlMhx26GpmiVP8755I2AMPGSl1h8YPF/vMpTmQAAAAABJRU5ErkJggg==️'
  elif icon_code == 'clear-night':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSI2eftoTwAAAP1JREFUOMvt1L8uBFEUx/GPJdkoJN4C2ymUEjWNRGErbKFV0W+v2Yi34AVEIVEoNESCB1CpBCGbYEdzio3c3Z3ZIZr9JTeTOX++c+fccy4j/bXGBvjHMY15ZLjEGzplPrqB6wB2r3OsDfsXLXwF6Bh1bOIkbJ/YKQpejeQ7zCb8i3iImIUi4DZeo7a91AjwY17oSiQc9jnY3Yh5irhc2oukrR7+WtT+GXP9QJUf7xMD2nA/cpZwX6S+y7HjgwS8Gr6zYfu3jZfE4dUC3MwDqSRsdUzhAjNd9vd4TpYZ81aMbYYjrMeAZLgte49s4yox0qe/fQl94CZarWOkf9c3P/Q+E7My3KMAAAAASUVORK5CYII='
  elif icon_code == 'rain':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSgTyBBUggAAAXtJREFUOMu11L1rVFEQBfBfwqrRRkIMaqOVBENEbYJi/PgTLCyCYG1hsLYUGxHxLxBJLyhYisRgaSCkUCuJWPgREkKQRKOruDYn4bHsw7fP9cBlLndmzr3vvJnpU449GEIDP7CKwawGvmEZ6yqgL/YaFrAW0mXM4RNaWU28xhUVMZ3EdbzCYyxiE59xH7cwi43EXv8b6c0EzmKkzTeO3W1nY3iTnItlpHvxAd+zr4px/MJz7OwUcDQ339Y93keqg1sH/QXnpdiZGsR3MYBHnZy/8/cH1MOTfPGJ4ovPp9QeROM6eBh7oUi8L/aj+mgWimCbeDH22D8Qn4ydb3esRKMjNUhHkrtRVo+ttPENHIpEwyVkZ3EG99KlLZwuu3kKXwvzYKu1R7ELOzKcpttivuByp6FTxGja8zgO4FySX+bSMezPUHqKt3iGpW71m0ir/8xq4o4eYhiHqwb3d0G8Emkm/Qe8S9U0ekl6qlAFV3v94hepjqFeEzeqyvAHYsBcWSzTMXUAAAAASUVORK5CYII='
  elif icon_code == 'snow':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSkMXANoNgAAAX1JREFUOMu91D9rFGEQBvDfSaKJjYhKTKOVhBwRtQmIRO2DkMJK8BMoqS2DjYj4CSSksxAULEU0IFgoiIVaSSCF8U9ERBKjHuLaPJHlvCWXPeMDL7O8M/PM7rMz01CNndiDPvzAJ+zO6cMalrGiCzRiL+A5Pod0GU/xFkVOCy9xXpeYS+IKXuAOFvAN73ADlzGP1cROb0Q6k8B5jLT5xjHYdjeGV8mZqiLdhTf4nuduMY6feIjtnQJGU/mKzWMxUg2vX2wrOc/GPqhBfA0DuN3J+St/f0A93M0XHy2/8am02mw0roNbsafLxHtjl9RHq9QEf4gXYg/3QHws9lm742M0OlSDdCS5q1X9WGSML+FAJNpXQTaBE7ieKS1wvKryRXwt7YP10W5iB/qznObaYr7gXKelU0Yz43kE+3EyyU9SdAxDWUr38Br38X6z+j3Kiiwyui1c1SP68Tiki9H1n2AQZ6JhYQsw6T+gsRWksxmCZg/L6i8cxIdofXMj4t/lkV9CmfnoWgAAAABJRU5ErkJggg=='
  elif icon_code == 'sleet':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSgsfnZ5vwAAAX9JREFUOMu11D9rFGEQBvBf4hmjjYgGtREbCYaI2gSCUfMRLKwEP0HE2lK0sBAL6yDpBQVrSVKEFAqSQu0MWBiVBJGQ+O8Uz+aJLOdtsrcxDwyz+87MM/vOzkyPcuzDQTTwA59wINLAVyxjTQX0RE9gAZ9DuozneI9WpIlXuKoiphK4hpd4jEV8wwdM4hZmsR7f61uR3ozjLAbbbCPY23Y2jNeJuVRGuh/v8D3PVTGCX5hBXyeHk8l8R/d4m1Id3TjoLRgvR0/XIL6LfjzqZPydv9+vHp7kxmeKX3wxrfYgNa6Dh9HjReJD0Uvqo1logr/Ei9GntkF8NvpFu2ElNTpRg3Qwsetl/djKGN/AsZRooITsPM7hXqa0hdGyzNfwpbAPNkZ7CHuwO8tpqs1nFVc6LZ0ihjKep3EEFxL8LEmHcThnt/EGT/Gx2/qNZdR/Rpq4j3n/CQM4Xnif28y50QXxSkSV6ey1Q9iKeDKdsN3b/oP5kmvv2myxV8Fc3W33B5OEWu82xu/1AAAAAElFTkSuQmCC'
  elif icon_code == 'wind':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSkhGdw0QwAAANJJREFUOMvt0zFOQlEQheFPtLEBaSWxIkglhWzAuAotjGyAVndAbNgBC9DOgoYF0ElBYkzsoTGihYkCEWymeCFU+AqK9yc3k5k7OTlzJ5eMjFV2U9Ip4A4VPGGRlsELLONMcJpLSfggBKGIxzREb8LpF7poRf4v9jDEFIdRu91EOI8a2ijH8gf4xRHqsbgPeMB1NK6jjCvc4yWxpMu4b0b+E3GBc5glmpd4xyhisj7HJ3rhLEkDfXRQhR2cxXgnOEYJ+/jGGK8x7nO851v2/TO2jD+UFjRuVLy76gAAAABJRU5ErkJggg=='
  elif icon_code == 'fog':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBRs3AHjVYwAAAVpJREFUOMvV1E0rZ2EYBvCf92ZKSlJkMU1hw1JZspbNSNlY+AJ2M6VJ+QgaC6SUlY0d2VlZ8QWYECVhmkyjLAZ/bG515uTvnEMWrnrqOc+57+t+ee7r4Y1QUcB2GD2x38TqSwK24gdOcYFL3KfWDpqLkA4miC7xK/bTGMIXLMTZX8yhKou0PRzuMJLRsu+4TgR4lnw2SMdzVleHpSBfLHdntdjADT4VaF1lZL6P+uRhEtUR/bYA8R1+ogU1TxFf4zDI+wpO0Wf8QamcQW9kfBbGj6jCxzI+k+GzknWBo2H4G1MxHWs4wAC6sBwC2Y1WlNCWp7SJKC0piFLq+zZsttFURNId6EYD9iKz/rj5LRzH/O7Hv/eHiqxWNONrCCWvQE4wk2X47YmXLM/6kJVxZVJBOXEfAvtPwmnUoPM5FaVwhaM84zaPsXQGGWjMk/F6zPC/nKTnBap7PR4Ao49mLsA0+N4AAAAASUVORK5CYII='
  elif icon_code == 'cloudy':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSMSXOO93wAAAXpJREFUOMvd1U9LFlEUBvBfQ2j0xxa2KFyI0KpauAikTbToE0iJX6A2fYFAqk0QhLXToqCVfQIJIlrmopB2im7CIoioiCw1Mps2zxvD25jhvJs6cJmZc899zr1znvNc/jXb8Ye5vehGgXWs5H0PduIHvuLL3ybbh1tYwAZKfMAjzCRBmbGICezeCvRIdlfiGx5iCvNJso7nuI3pxJTxH94MtAevEziWY1ftBI61+QpcyZoXm+38RgIubKNWF7P2ct1kibkGRHiZgv46CpzKc7oB8IOw7GQV/HGyjTQAPpsCPwkd9WM59Gpqz7CGgQIHwt17HQCewi70Fdgf53IHgN+3urbAm3wc7wBwq3CvoDdU+YRDDUAP4mOwelvOq+HxU3RtIU51QtaF2WBca1e3OziHz7iPt6Hi9RoF68YlfA+rzkQN7+K8mr4frShXa7zDUCXuKJbaYtYwXKMvv9lQkoynccpo72oFbBKnMbjdi6A//20242Z8hf/KfgLcrl6xBGFPnAAAAABJRU5ErkJggg=='
  elif icon_code == 'partly-cloudy-day':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSMmfVdJagAAAaBJREFUOMvN1M+LjlEUB/DPmMkgFopBqUFZkZpimpXyI8VGFoosmNhZyU4p2crWggXiT0AskB+ZmpKFolB+NGEWZDKmNPTafBe3p/d9533e2Th16rn3fs+533PO9z7Utw34jn3tQAu6SLwNyzE0n8Sb8BSDxd4LzOBtsbcI73CmU3Z70cAnDLTBvQ7uVJ3ST2MCK8L8LmbxuUh0FhfQ00VrHQyrBt5jOt/3zMNWh+EMtqMv7B62akEr6sM4is34gykcwHUcq8T/jK/BxahlvFnSE0XJVb+DnQW2N22ZzXoyuOcl4DDe5OAbRnPWh+P4VVwwhd3Y0aTP/aWWrxZB97GrSSUj2IPzwU0Xlw1qEdDAR6zqcJhD+Bs/hJXYWgVdTuKNNVTSW+h5PW4nx8kS9Cybde1aFDMcOf7Ab8VgJvK9NH2by3riWzKfSXzNur8E7g/jJzXYnkvMo3agxXgc4FgHSdcG+wHLOinvQQJuVH6rl7Cw0OhYcKN1Jv0yQa9wM6+qkemPF7q9Ve3nXLYkjMtnfCUDbuALjmToXds6/6P9AxT9dR9KWJuDAAAAAElFTkSuQmCC'
  elif icon_code == 'partly-cloudy-night':
    icon = 'iVBORw0KGgoAAAANSUhEUgAAABYAAAAWCAYAAADEtGw7AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAABYlAAAWJQFJUiTwAAAAB3RJTUUH4AobBSM6aVYVJQAAAXVJREFUOMvt1b9L1VEYx/HXzR8hEUVbChEmQUMRVouLc1ibQz+maGryLyiiwWivoSUXsdUfIGhNTbU0hKAIQUMk9ONSgqhZXpfP8OVyr329NTh04MD3+5zPeZ/nnPM8z+F/S6u0MOcWzuI35vHib53owyZqgW7new4drUJP4HtAj3EF1wKt4VWr4NEAHtTZD+JjxobLnv1h3MYzVDP5QkHTieXYX6J3N+A9fMIXfMukYv+J+9Heje1pFmnYjuFdhF/xHqt4jvMZv4kP0YwkIrZwphm0vXAJT9AT0Em01WlPZ8FtbGQHR5uBTyWMlkte5NWC579wqZHoALpzRmMlwTPox1Jhxw3Bm/nu2kP4VRMJlTjWEPwZa7i+x9jeCripxz+wkksom5qHMJjUXmgm6sAbrOPiH4DtuIzZXN5EmYpVS1IMxDaIO3W68UKyTJU9s4eFilXsE8my1/l/ixutlMZHmEzhWaxbZBpH/tVjcA5DOL4vnqYd/4Bh/ET7XIsAAAAASUVORK5CYII='
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
    print weather_data['temperature'] + ' | templateImage=' + weather_data['icon']
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