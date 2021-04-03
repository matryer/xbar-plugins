#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>GitHub Zen</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Josh</xbar.author>
# <xbar.author.github>andjosh</xbar.author.github>
# <xbar.desc>GitHub zen in your menu bar!</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>http://i.imgur.com/U4OHxDm.png</xbar.image>

import urllib2
import os

api_key = os.getenv('GITHUB_TOKEN', 'Enter your GitHub.com Personal Access Token here...')
url = 'https://api.github.com/zen'

request = urllib2.Request( url, headers = { 'Authorization': 'token ' + api_key } )
response = urllib2.urlopen( request )
print ( '%s' % (response.read())).encode( 'utf-8' )
