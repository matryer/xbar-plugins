#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Codeship</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Latest build status for all projects in Codeship</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/683200/16809335/fadc3746-48ed-11e6-8c86-517dd94412ff.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2

# Get it here: https://codeship.com/user/edit
API_KEY = ''

BASE_URL = 'https://codeship.com/api/v1/projects.json?api_key=' + API_KEY
COLORS = {
  'success': '#42A86F',
  'testing': '#607192',
  'error': '#D12C3F',
  'stopped': '#C4CDCE'
}

def getData():
  try:
    return json.load(urllib2.urlopen(BASE_URL))
  except urllib2.URLError:
    return False

def generateMenu(data):
  if data is False:
    print 'Build data unavailable'
    return False

  for project in data['projects']:
    print project['repository_name'] + ' | href=' + 'https://codeship.com/projects/' + str(project['id'])
    print '---'
    for build in project['builds']:
      print '- ' + build['branch'] + ': ' + build['status']  + ' | color=' + COLORS[build['status']] + ' href=' + 'https://codeship.com/projects/' + str(build['project_id']) + '/builds/' + str(build['id'])
    print '---'

print 'Codeship'
print '---'
generateMenu(getData());
