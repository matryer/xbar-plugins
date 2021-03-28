#!/usr/bin/env python

# <xbar.title>Songkick - Upcoming events</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>Nobuhide Yayoshi</xbar.author>
# <xbar.author.github>yayoc</xbar.author.github>
# <xbar.desc>Upcoming music events on Songkick</xbar.desc>
# <xbar.image>http://i.imgur.com/pu8gJh2.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.songkick.com/</xbar.abouturl>

import json
import urllib2

def get_events():
    try:
        ip = get_my_ip()
        request = urllib2.Request('http://api.songkick.com/api/3.0/events.json?apikey=' + configs['apikey'] + '&location=ip:' + ip)
        response = urllib2.urlopen(request)
        response = json.load(response)
        return map(format_events, response['resultsPage']['results']['event'])
    except Exception:
        print ':('

def format_events(event):
    return (u'%s / %s | href=%s' % (event['displayName'], event['location']['city'], event['uri'])).encode('utf-8')

def get_my_ip():
    request = urllib2.Request('http://ip.42.pl/raw')
    response = urllib2.urlopen(request)
    return response.read()

configs = {
    'apikey': 'Your API Key'
}

print (u'\u24c8 | size=18').encode('utf-8')
print '---'
print '\n'.join(get_events())
print '---'
print 'Songkick - Upcoming events | href=https://songkick.com'


