#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Codeship Single Project</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Daniel Seripap</bitbar.author>
# <bitbar.author.github>seripap</bitbar.author.github>
# <bitbar.desc>Latest build status for individual Codeship project</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/683200/16809236/8fffa28c-48ed-11e6-9060-e42992612f50.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2

# Get it here: https://codeship.com/user/edit
API_KEY = ''
# Modify accordingly
PROJECT_ID = 0

BASE_URL = 'https://codeship.com/api/v1/projects.json?api_key=' + API_KEY
HREF_URL = 'https://codeship.com/projects/' + str(PROJECT_ID) + '/builds/'
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

def generate(data):
  header = []
  menu = []
  if data is False:
    print 'Build data unavailable'
    return False

  for project in data['projects']:
    if PROJECT_ID == project['id']:
      index = 0;
      latest = False;
      for build in project['builds']:
        menu.append('- ' + build['branch'] + ': ' + build['status'] + ' | color=' + COLORS[build['status']] + ' href=' + HREF_URL + str(build['id']))
        if index == 0:
          index = 1
          latest = COLORS[build['status']]

      header.append(project['repository_name'] + ' | color=' + latest)
      header.append('---')

  for line in header:
      print line.encode('utf-8')
  for line in menu:
      print line.encode('utf-8')

generate(getData());
