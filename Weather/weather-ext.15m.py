#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Weather - OpenWeatherMap</bitbar.title>
# <bitbar.version>v1.0.2</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Grabs simple weather information from openweathermap. Translates to Needs configuration for location and API key.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
from random import randint

location = '2193733'
api_key = '8b4824b451d5db1612156837df880f55'
units = 'metric' # kelvin, metric, imperial
lang = 'en'
img = ''

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
  
 # 01 ☀️ Numbers from Open Weather's Description Weather Icons
 # 02 🌤️ https://openweathermap.org/weather-conditions
 # 03 ⛅
 # 04 🌥️ 🌂 🍃
 # 09 🌧️ ☔
 # 10 🌦️ ☂️
 # 11 🌩️ ⛈️ ☔
 # 13 ❄️
 # 50 🌁 🌫️ 🌋 🌪️
 # Don't Know 🤷‍♂️
  
def weathericonday():
  weather_data = get_wx()
  if weather_data['condition'] == "clear sky":     
    icon = "☀️" 
  elif weather_data['condition'] ==  "few clouds":    
    icon = "🌤️" 
  elif weather_data['condition'] ==  "scattered clouds": 
    icon = "⛅"
  elif weather_data['condition'] ==  "broken clouds":
    icon = "🌥️"                  
  elif weather_data['condition'] ==  "shower rain":    
    icon = "🌧️"
  elif weather_data['condition'] ==  "rain":           
    icon = "🌦️"
  elif weather_data['condition'] ==  "thunderstorm":   
    icon = "🌩️"
  elif weather_data['condition'] ==  "snow":                 
    icon = "❄️"
  elif weather_data['condition'] ==  "mist":               
    icon = "🌫️"
  elif weather_data['condition'] ==  "thunderstorm with light rain":       
    icon = "⛈️"
  elif weather_data['condition'] ==  "thunderstorm with rain": 
    icon = "⛈️"
  elif weather_data['condition'] ==  "thunderstorm with heavy rain": 
    icon = "⛈️"
  elif weather_data['condition'] ==  "light thunderstorm":       
    icon = "🌩️"
  elif weather_data['condition'] ==  "heavy thunderstorm": 
    icon = "🌩️"
  elif weather_data['condition'] ==  "ragged thunderstorm":       
    icon = "🌩️"
  elif weather_data['condition'] ==  "thunderstorm with light drizzle":       
    icon = "⛈️"
  elif weather_data['condition'] ==  "thunderstorm with drizzle":       
    icon = "⛈️"
  elif weather_data['condition'] ==  "thunderstorm with heavy drizzle":       
    icon = "⛈️"
  elif weather_data['condition'] ==  "light intensity drizzle":       
    icon = "🌧️"                                               
  elif weather_data['condition'] ==  "drizzle":            
    icon = "🌧️"
  elif weather_data['condition'] ==  "heavy intensity drizzle":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "light intensity drizzle rain":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "drizzle rain":         
    icon = "🌧️"
  elif weather_data['condition'] ==  "heavy intensity drizzle rain": 
    icon = "🌧️"
  elif weather_data['condition'] ==  "shower rain and drizzle ":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "heavy shower rain and drizzle":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "shower drizzle":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "light rain":           
    icon = "🌦️"
  elif weather_data['condition'] ==  "moderate rain":                 
    icon = "🌦️"
  elif weather_data['condition'] ==  "heavy intensity rain":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "very heavy rain":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "extreme rain":         
    icon = "🌧️"
  elif weather_data['condition'] ==  "freezing rain":        
    icon = "❄️"
  elif weather_data['condition'] ==  "light intensity shower rain": 
    icon = "🌧️"
  elif weather_data['condition'] ==  "ragged shower rain":       
    icon = "🌧️"
  elif weather_data['condition'] ==  "light snow":           
    icon = "❄️"
  elif weather_data['condition'] ==  "Heavy snow":           
    icon = "❄️❄️"
  elif weather_data['condition'] ==  "Sleet":                
    icon = "❄️🌧️"
  elif weather_data['condition'] ==  "Light shower sleet":       
    icon = "❄️"
  elif weather_data['condition'] ==  "Shower sleet":       
    icon = "❄️🌧️"
  elif weather_data['condition'] ==  "Light rain and snow":       
    icon = "❄️"
  elif weather_data['condition'] ==  "Rain and snow":        
    icon = "❄️🌧️"
  elif weather_data['condition'] ==  "Light shower snow":       
    icon = "❄️"
  elif weather_data['condition'] ==  "Shower snow":          
    icon = "❄️"
  elif weather_data['condition'] ==  "Heavy shower snow":       
    icon = "❄️❄️"
  elif weather_data['condition'] ==  "Smoke":               
    icon = "🌋"
  elif weather_data['condition'] ==  "Haze":                 
    icon = "🌫️"
  elif weather_data['condition'] ==  "sand/ dust whirls":       
    icon = "🌫️🌪️"
  elif weather_data['condition'] ==  "Dust":                 
    icon = "🌫️"
  elif weather_data['condition'] ==  "dust":                 
    icon = "🌫️"
  elif weather_data['condition'] ==  "fog":                  
    icon = "🌁"
  elif weather_data['condition'] ==  "sand":                 
    icon = "🌫️"
  elif weather_data['condition'] ==  "volcanic ash":         
    icon = "🌋"
  elif weather_data['condition'] ==  "squalls":              
    icon = "🌪️"
  elif weather_data['condition'] ==  "tornado":              
    icon = "🌪️"
  elif weather_data['condition'] ==  "Ash":                  
    icon = "🌋"
  elif weather_data['condition'] ==  "Squall":               
    icon = "🌪️"
  elif weather_data['condition'] ==  "overcast clouds":       
    icon = "🌥️"
  else:
    icon ="🤷‍♂️"
  return icon
  
print weathericonday()
print "---\n"
print render_wx()
