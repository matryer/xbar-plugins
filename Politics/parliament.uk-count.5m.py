#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Parliament.uk Count</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Keith Cirkel</xbar.author>
# <xbar.author.github>keithamus</xbar.author.github>
# <xbar.desc>See the vote count for a Parliament.uk petition</xbar.desc>
# <xbar.image>iVBORw0KGgoAAAANSUhEUgAAAKEAAABRAQMAAACADVTsAAAABlBMVEUiIiL///9ehyAxAAABrElEQVR4Xu3QL2/bQBgG8NdRlrnMNqxu1eVAahCQVAEF03STbsuBSFVZYEBBoJ2RjZ0Hljuy6IZaUlUlpfsKRUmZP4JTNJixkEm7nJu/Mxlot0l7JJOfXj06P/D3xvkBQH/lqoEC7WVvzqM0k/f4+Gat2nt7ppqeCjCbiJX6HmN7vnca4LLc0BljH/yZ0ZejDQXGlA9GmYSthoumVw1wZ6PByxjrpxmeZq0hbMcDXPCHGVB4hHCAkgUKrrNSulawelPRCH37mu4fR1EdZYPwnTA6UZoQfteoMSmPCFVcgYmUmmCuPMKkIAtNFjqS+hWyOo+MzmVsb12NS1aFazThe1Ztr2qYBklWvcPKCKG+TA/MGwjqDcI4n1Pko+1E5KM9TRz75fGB0qWv1Vlq/Bo9Gzqo3oqu7g991G1bVQmp8IQcdeRtEGpyxoVVB5eNLob0qS6xpaJc5+J7Wx+wkwct5SoSn2vCOORKrHZk0lC69tAbm4a2g0grEuknvd9tb61XhqK8hz+d/xG/cft5fD0dvxA7qsLrj+EXWqBugRbeHl6qcbCr4Ba+7Tn88/kJk4CIztd1IrIAAAAASUVORK5CYII=</xbar.image>
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
