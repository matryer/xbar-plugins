#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>OWM_Weather</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jack Zhang</bitbar.author>
# <bitbar.author.github>JAckZ97</bitbar.author.github>
# <bitbar.desc>Using weather information from openweathermap (OWM). Require signup for personal API key and your own locations id. Information detail inside the source code</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/JAckZ97/Bitbar_weather_plugin/master/img/src_image.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/JAckZ97/Bitbar_weather_plugin</bitbar.abouturl>

import json
import urllib2
from random import randint

# Search your own city id by: https://openweathermap.org/city
location = '6077243'                            # Montreal City id
api_key = '8b4824b451d5db1612156837df880f55'    # OWM API key
units = 'metric'                                # kelvin, metric, imperial
lang = 'en'


def get_wx():

    if api_key == "":
        return False

    try:
        wx = json.load(urllib2.urlopen('http://api.openweathermap.org/data/2.5/weather?id=' + location +
                                       '&units=' + units + '&lang=' + lang + '&appid=' + api_key + "&v=" + str(randint(0, 100))))

    except urllib2.URLError:
        return False

    if units == 'metric':
        unit = 'C'
    elif units == 'imperial':
        unit = 'F'
    else:
        unit = 'K'  # Default is kelvin

    try:
        weather_data = {
            'temperature': str(int(round(wx['main']['temp']))),
            'feels_like': str(int(round(wx['main']['feels_like']))),
            'temp_min': str(int(round(wx['main']['temp_min']))),
            'temp_max': str(int(round(wx['main']['temp_max']))),
            'pressure': str(int(round(wx['main']['pressure']))),
            'humidity': str(int(round(wx['main']['humidity']))),
            'wind_speed': str(int(round(wx['wind']['speed']))),
            'wind_deg': str(int(round(wx['wind']['deg']))),
            'weather_icon': wx['weather'][0]['icon'],
            'weather_description': wx['weather'][0]['description'],
            'city': wx['name'],
            'unit': '°' + unit
        }
    except KeyError:
        return False

    return weather_data


def render_wx():
    weather_data = get_wx()

    tridash = '\n' + '---' + '\n'
    if weather_data is False:
        return 'Err' + tridash + 'Could not get weather; Maybe check API key or location?'

    weather_temp_with_unit = weather_data['temperature'] + weather_data['unit']
    return weather_temp_with_unit


def render_wx_detail_temp():
    weather_data = get_wx()

    weather_temp_details = "Temp_min: " + \
        weather_data['temp_min'] + weather_data['unit'] + "Temp_max: " + \
        weather_data['temp_max'] + weather_data['unit']+"Feels like: " + \
        weather_data['feels_like'] + weather_data['unit']

    return weather_temp_details


weather_data = get_wx()


print("MTL temp: " + render_wx()+"☁️")
print("---")
print("Feels like: " + weather_data['feels_like'] + weather_data['unit'])
print("Temp_min: " + weather_data['temp_min'] + weather_data['unit'] +
      " / " + "Temp_max: " + weather_data['temp_max'] + weather_data['unit'])
print("---")
print("Description: " + weather_data['weather_description'])
print("Humidity: " + weather_data['humidity']+"%")
print("Pressure: " + weather_data['pressure']+" hPa")
print("Wind_speed: " + weather_data['wind_speed'])
print("Wind_deg: " + weather_data['wind_deg'])

