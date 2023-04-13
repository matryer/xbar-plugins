#!/usr/bin/env python3

# <xbar.title>Weather - OpenWeatherMap</xbar.title>
# <xbar.version>v1.3</xbar.version>
# <xbar.author>Daniel Seripap</xbar.author>
# <xbar.author.github>seripap</xbar.author.github>
# <xbar.desc>Grabs simple weather information from openweathermap. Needs configuration for location and API key.</xbar.desc>
# <xbar.image>https://poolis.github.io/bitbar-plugins/open-weather-preview.png</xbar.image>
# <xbar.dependencies>python,emoji</xbar.dependencies>

import emoji
import json
from urllib.request import urlopen
from urllib.error import URLError
from random import randint
import datetime

def get_location_using_ip():
    service_endpoint = 'http://ip-api.com/json'
    r = urlopen(service_endpoint).read()
    j = json.loads(r)
    city = j['city']
    country = j['countryCode']
    location = f"{city},{country}"

    return location.replace(" ", "%20")

location_name = get_location_using_ip()
api_key = '8b4824b451d5db1612156837df880f55'
units = 'imperial'  # kelvin, metric, imperial
lang = 'en'


def get_wx():
    if api_key == "":
        return False

    try:
        daily_wx = json.load(urlopen(f'http://api.openweathermap.org/data/2.5/forecast/daily?q={location_name}'
                                     f'&units={units}&lang={lang}&appid={api_key}&v={str(randint(0, 100))}'))
        location = str(daily_wx['city']['id'])
        wx = json.load(urlopen(
            'http://api.openweathermap.org/data/2.5/weather?id=' + location + '&units=' + units + '&lang=' + lang + '&appid=' + api_key + "&v=" + str(
                randint(0, 100))))
    except URLError:
        return False

    if units == 'metric':
        unit = 'C'
    elif units == 'imperial':
        unit = 'F'
    else:
        unit = 'K'  # Default is kelvin

    try:
        daily_forecast = []
        for day in daily_wx['list']:
            daily_forecast.append({'id': day['weather'][0]['id'],
                                   'datetime': datetime.datetime.fromtimestamp(day['dt']),
                                   'max': str(int(round(day['temp']['max']))),
                                   'min': str(int(round(day['temp']['min'])))})
        weather_data = {
            'temperature': str(int(round(wx['main']['temp']))),
            'condition': str(wx['weather'][0]['description']),
            'id': wx['weather'][0]['id'],
            'city': wx['name'],
            'unit': '°' + unit,
            'daily_forecast': daily_forecast
        }
    except KeyError:
        return False

    return weather_data


def render_wx():
    weather_data = get_wx()
    emoji_dict = {
        200: ":zap:", 201: ":zap:", 202: ":zap:", 210: ":zap:", 211: ":zap:", 212: ":zap:", 221: ":zap:", 230: ":zap:",
        231: ":zap:", 232: ":zap:",
        300: ":umbrella:", 301: ":umbrella:", 302: ":umbrella:", 310: ":umbrella:", 311: ":umbrella:",
        312: ":umbrella:", 313: ":umbrella:", 314: ":umbrella:", 321: ":umbrella:",
        500: ":umbrella:", 501: ":umbrella:", 502: ":umbrella:", 503: ":umbrella:", 504: ":umbrella:",
        511: ":umbrella:", 520: ":umbrella:", 521: ":umbrella:", 522: ":umbrella:", 531: ":umbrella:",
        600: ":snowflake:", 601: ":snowflake:", 602: ":snowflake:", 611: ":snowflake:", 612: ":snowflake:",
        613: ":snowflake:", 615: ":snowflake:", 616: ":snowflake:", 620: ":snowflake:", 621: ":snowflake:",
        622: ":snowflake:",
        701: ":fog:", 711: ":fog:", 721: ":fog:", 731: ":fog:", 741: ":fog:", 751: ":fog:", 761: ":fog:", 762: ":fog:",
        771: ":fog:",
        781: ":cyclone:",
        800: ":sunny:",
        801: ":partly_sunny:", 802: ":partly_sunny:", 803: ":cloud:", 804: ":cloud:",
    }
    tridash = '\n' + '---' + '\n'

    if weather_data is False:
        return 'Err' + tridash + 'Could not get weather; Maybe check API key or location?'

    emojiweather = emoji.emojize(emoji_dict[weather_data['id']])

    emoji_t = '' + emojiweather + weather_data['temperature'] + weather_data['unit']
    condi = [x.capitalize() for x in weather_data['condition'].split(' ')]
    daily_forecast_encoded = '\n'
    for daily_forecast in weather_data['daily_forecast']:
        daily_forecast_encoded = f"{daily_forecast_encoded}{daily_forecast['datetime'].strftime('%a')} " \
                                 f"{daily_forecast['datetime'].month}/{daily_forecast['datetime'].day} " \
                                 f"{emoji.emojize(emoji_dict[daily_forecast['id']])} " \
                                 f"{daily_forecast['max']}{weather_data['unit']}/" \
                                 f"{daily_forecast['min']}{weather_data['unit']} | font=Menlo color=white\n"
    return f'{emoji_t}{tridash}{" ".join(condi)} | {daily_forecast_encoded}'


print(render_wx())
