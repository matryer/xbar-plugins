#!/usr/bin/env LC_ALL=en_US.UTF-8 /usr/local/bin/python3
#
# <xbar.title>Yahoo Weather</xbar.title>
# <xbar.version>v3.0</xbar.version>
# <xbar.author>mgjo5899</xbar.author>
# <xbar.author.github>mgjo5899</xbar.author.github>
# <xbar.desc>It tells you the current weather condition of the location where your computer is located at.  It knows the location of the computer by using its public IP.  You can also manually set the city and region through modifying the file. </xbar.desc>
# <xbar.image>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</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
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
