#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python3
#
# <bitbar.title>Yahoo Weather</bitbar.title>
# <bitbar.version>v3.0</bitbar.version>
# <bitbar.author>mgjo5899</bitbar.author>
# <bitbar.author.github>mgjo5899</bitbar.author.github>
# <bitbar.desc>It tells you the current weather condition of the location where your computer is located at.  It knows the location of the computer by using its public IP.  You can also manually set the city and region through modifying the file. </bitbar.desc>
# <bitbar.image>https://i.imgur.com/YNypf0P.jpg</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
#
# by mgjo5899

import json, uuid, time, hmac, hashlib

from base64 import b64encode
from urllib.request import urlopen, Request
from urllib.parse import urlencode, quote

# Change unit to 'c' for celsius and 'f' for fahrenheit
unit = 'c'

# General Placeholders
url = 'https://weather-ydn-yql.media.yahoo.com/forecastrss'
method = 'GET'
concat = '&'

# Credentials
app_id = 'f776QQ32'
consumer_key = 'dj0yJmk9RlJhbUVpUEpsSUxEJmQ9WVdrOVpqYzNObEZSTXpJbWNHbzlNQS0tJnM9Y29uc3VtZXJzZWNyZXQmc3Y9MCZ4PTk0'
consumer_secret = '75c592717d22c5cce623d2c2a1d5a5b36786d865'

# Query and authentication related
query = {'location': 'seoul,korea', 'format': 'json', 'u': unit}
oauth = {
    'oauth_consumer_key': consumer_key,
    'oauth_nonce': uuid.uuid4().hex,
    'oauth_signature_method': 'HMAC-SHA1',
    'oauth_timestamp': str(int(time.time())),
    'oauth_version': '1.0'
}


# Error handling decorator
def exception_handler(msg="Something is wrong"):
    def decorator(func):
        def new_func(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            except:
                print(f"Error: {msg}")
                exit(1)
        return new_func
    return decorator


@exception_handler(msg="Location service")
def get_location_using_ip():
    service_endpoint = 'http://ip-api.com/json'
    r = urlopen(service_endpoint).read()
    j = json.loads(r)
    city = j['city']
    region = j['region']

    return f"{city},{region}"


def get_auth_header():
    global oauth
    merged_params = query.copy()
    merged_params.update(oauth)
    sorted_params = [k + '=' + quote(merged_params[k], safe='') for k in sorted(merged_params.keys())]
    signature_base_str =  method + concat + quote(url, safe='') + concat + quote(concat.join(sorted_params))
    composite_key = quote(consumer_secret, safe='') + concat
    oauth_signature = b64encode(hmac.new(composite_key.encode(), msg=signature_base_str.encode(), digestmod=hashlib.sha1).digest()).decode()
    oauth['oauth_signature'] = oauth_signature
    auth_header = 'OAuth ' + ', '.join(['{}="{}"'.format(k,v) for k,v in oauth.items()])

    return auth_header


@exception_handler(msg="Yahoo Weather API")
def get_weather(auth_header):
    request_url = url + '?' + urlencode(query)
    request = Request(request_url)
    request.add_header('Authorization', auth_header)
    request.add_header('X-Yahoo-App-Id', app_id)
    r = urlopen(request).read()
    j = json.loads(r)
    condition_data = j['current_observation']['condition']
    condition = condition_data['text']
    temperature = condition_data['temperature']

    return (condition, temperature)

location = get_location_using_ip()
query['location'] = location
auth_header = get_auth_header()
condition, temperature = get_weather(auth_header)

if unit == 'c':
  print(str(condition) + ' : ' + str(int(temperature)) + '°C')
elif unit == 'f':
  print(str(condition) + ' : ' + str(int(temperature)) + '°F')
