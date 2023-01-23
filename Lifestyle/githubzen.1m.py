#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>GitHub Zen</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Josh</xbar.author>
# <xbar.author.github>andjosh</xbar.author.github>
# <xbar.desc>GitHub zen in your menu bar!</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.image>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</xbar.image>

import urllib2
import os

api_key = os.getenv('GITHUB_TOKEN', 'Enter your GitHub.com Personal Access Token here...')
url = 'https://api.github.com/zen'

request = urllib2.Request( url, headers = { 'Authorization': 'token ' + api_key } )
response = urllib2.urlopen( request )
print ( '%s' % (response.read())).encode( 'utf-8' )
