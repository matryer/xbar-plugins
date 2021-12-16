#!/usr/bin/python
#
# <xbar.author>Till Dettmering</xbar.author>
# <xbar.author.github>dettmering</xbar.author.github>
# <xbar.title>Get steps and sleep duration from your Jawbone UP</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.desc>Get today's steps and sleep duration from your Jawbone UP!</xbar.desc>
# <xbar.image>http://i.imgur.com/JtguThL.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>http://tilldettmering.com/</xbar.abouturl>

import json
import urllib2

# You have to follow these steps to request an access token:
# https://jawbone.com/up/developer/authentication
# Set your access token below:

access_token = "my token"

# Function for retrieving data:

def retrieveUpData(type):
    url = "https://jawbone.com/nudge/api/v.1.1/users/@me/{0}".format(type)

    request = urllib2.Request(url)
    request.add_header("Authorization", "Bearer {0}".format(access_token))

    response = urllib2.urlopen(request)
    data = json.loads(response.read())

    return data
    
# Get steps and distance walked:

step_data = retrieveUpData("moves")

steps = step_data['data']['items'][0]['details']['steps']
distance = step_data['data']['items'][0]['details']['km']

# Get hours of sleep:

sleep_data = retrieveUpData("sleeps")

sleep_duration = sleep_data['data']['items'][0]['details']['duration'] / 3600
sleep_percentage = sleep_duration / 24.0 * 100

# Format output:

print '{0} steps'.format(steps)
print '---'
print '{0} km'.format(round(distance, 1))
print '{0} h slept ({1}%)'.format(round(sleep_duration, 1), round(sleep_percentage, 0))
