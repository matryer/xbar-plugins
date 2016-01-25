#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>GitHub Zen</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Josh</bitbar.author>
# <bitbar.author.github>andjosh</bitbar.author.github>
# <bitbar.desc>GitHub zen in your menu bar!</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.image>http://i.imgur.com/U4OHxDm.png</bitbar.image>

import urllib2
import os

api_key = os.getenv('GITHUB_TOKEN', 'Enter your GitHub.com Personal Access Token here...')
url = 'https://api.github.com/zen'

request = urllib2.Request( url, headers = { 'Authorization': 'token ' + api_key } )
response = urllib2.urlopen( request )
print ( '%s' % (response.read())).encode( 'utf-8' )
