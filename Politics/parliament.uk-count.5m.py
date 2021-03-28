#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Parliament.uk Count</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Keith Cirkel</xbar.author>
# <xbar.author.github>keithamus</xbar.author.github>
# <xbar.desc>See the vote count for a Parliament.uk petition</xbar.desc>
# <xbar.image>https://i.imgur.com/I38h0UJ.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import json
import urllib2
import locale
locale.setlocale(locale.LC_ALL, 'en_GB')

id = '131215'
name = 'EU 2nd Ref'
constituents = False

request = urllib2.Request( 'https://petition.parliament.uk/petitions/' + id + '.json' )
response = urllib2.urlopen( request )
data = json.load( response )['data']['attributes']
print name + ': ' + locale.format('%d', data['signature_count'], grouping=True)
print '---'
if constituents:
  for cons in data['signatures_by_constituency']:
    print cons['name'] + ': ' + locale.format('%d', cons['signature_count'], grouping=True)
print 'Refresh | refresh=true'
