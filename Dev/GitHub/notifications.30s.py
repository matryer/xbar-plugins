#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Github Notifications by Keith Cirkel
# Fetch Github notifications in your menu bar!

import json
import urllib2
import os
import re

api_key = os.getenv('GITHUB_TOKEN', 'Please put your GitHub Token here')
url = 'https://api.github.com/notifications'

request = urllib2.Request( url, headers     = { 'Authorization': 'token ' + api_key } )
response = urllib2.urlopen( request )
notifications = json.load( response )

color = '#7d7d7d'
if len( notifications ) > 0:
    color='#4078C0'

print ( u'\u25CF | color=' + color ).encode( 'utf-8' )
print '---'
for notification in notifications:
    title = notification['subject']['title']
    repo = notification['repository']['full_name']
    url = re.sub( '^https://api.github.com/repos/', 'https://github.com/', notification['subject']['url'] )
    url = re.sub( '/pulls/', '/pull/', url )
    url = re.sub( '/commits/', '/commit/', url )
    print ( '%s: %s | refresh=true href=%s' % ( repo, title, url ) ).encode( 'utf-8' )
