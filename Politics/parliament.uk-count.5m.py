#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Parliament.uk Count</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>Keith Cirkel</bitbar.author>
# <bitbar.author.github>keithamus</bitbar.author.github>
# <bitbar.desc>See the vote count for a Parliament.uk petition</bitbar.desc>
# <bitbar.image>https://i.imgur.com/I38h0UJ.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

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
