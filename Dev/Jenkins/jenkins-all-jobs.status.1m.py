#!/usr/bin/env python

# <bitbar.title>Jenkins All Build Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dan Rassi</bitbar.author>
# <bitbar.author.github>drassi</bitbar.author.github>
# <bitbar.desc>Show current status of *all* jobs on a Jenkins instance, colors indicating status of each build and overall status. Clicks navigate to console output of last build.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/ARJHMjy.png</bitbar.image>
# <bitbar.dependencies>python,requests</bitbar.dependencies>

import sys
import requests
import collections

# Credentials to your jenkins instance
CREDS = {
    'USER' : 'user',
    'PASS' : 'pass',
'BASE_URL' : 'jenkins-url.com',
  'SCHEMA' : 'https',
}

# Text prefix for in-progress builds
IS_BUILDING_PREFIX = '*'
# Suffix appended to build URLs on click
URL_SUFFIX = 'lastBuild/console'
# Text displayed in the OSX menu bar
BAR_TEXT = 'build'

# Color defs @ https://github.com/jenkinsci/jenkins/blob/master/core/src/main/java/hudson/model/BallColor.java
color_map = collections.OrderedDict([
    ('grey'    , 'lightgray'),
    ('disabled', 'lightgray'),
    ('aborted' , 'lightgray'),
    ('notbuilt', 'lightgray'),
    ('blue'    ,  None), # leave default unspecified - black is picked in light mode, and white in dark mode
    ('yellow'  , 'yellow'),
    ('red'     , 'red'),
])

URL='{SCHEMA}://{USER}:{PASS}@{BASE_URL}/api/json?pretty=true'.format(**CREDS)

req = requests.get(URL)

if req.status_code != requests.codes.ok:
    sys.exit('error %d from jenkins!' % req.status_code)

output = ''
notify_val = 0

def get_color(job):
    is_building = '_anime' in job['color']
    color_val = color_map.keys().index(job['color'].replace('_anime',''))
    color = color_map.values()[color_val]
    return color, color_val, is_building

def get_color_def(color):
    return ('color=%s' % color) if color is not None else ''

for job in req.json()['jobs']:
    color, color_val, is_building = get_color(job)
    output += '%s%s|%s href=%s\n' % (IS_BUILDING_PREFIX if is_building else '',
                                     job['name'], get_color_def(color), job['url'] + URL_SUFFIX)
    notify_val = max(notify_val, color_val)

bar_color = color_map.values()[notify_val]

print BAR_TEXT, '|', get_color_def(bar_color)
print '---'
print output
