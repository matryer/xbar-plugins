#!/usr/bin/env python3

# <xbar.title>Parliament.uk Count</xbar.title>
# <xbar.version>v1.1.0</xbar.version>
# <xbar.author>Keith Cirkel</xbar.author>
# <xbar.author.github>keithamus</xbar.author.github>
# <xbar.desc>See the vote count for a Parliament.uk petition</xbar.desc>
# <xbar.image>https://i.imgur.com/I38h0UJ.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>

import json
import urllib.request, urllib.error, urllib.parse
import locale
locale.setlocale(locale.LC_ALL, 'en_GB')

id = '131215'
name = 'EU 2nd Ref'
constituents = False

request = urllib.request.Request( 'https://petition.parliament.uk/petitions/' + id + '.json' )
response = urllib.request.urlopen( request )
data = json.load( response )['data']['attributes']
print(name + ': ' + locale.format_string('%d', data['signature_count'], grouping=True))
print('---')
if constituents:
  for cons in data['signatures_by_constituency']:
    print(cons['name'] + ': ' + locale.format_string('%d', cons['signature_count'], grouping=True))
print('Refresh | refresh=true')
