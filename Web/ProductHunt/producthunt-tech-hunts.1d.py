#!/usr/bin/env python

# <bitbar.title>Product Hunt - Today in Tech</bitbar.title>
# <bitbar.version>v1.0.0</bitbar.version>
# <bitbar.author>John Flesch</bitbar.author>
# <bitbar.author.github>flesch</bitbar.author.github>
# <bitbar.desc>Today's featured tech hunts on Product Hunt</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/13259/12370591/3039c57e-bbdc-11e5-9b42-e4ab9f6bf851.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://www.producthunt.com/</bitbar.abouturl>

import json
import urllib2

def get_token():
    data = json.dumps(credentials)
    try:
        request = urllib2.Request('https://api.producthunt.com/v1/oauth/token', headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Content-Length': len(data.encode('utf-8'))
        })
        response = urllib2.urlopen(request, data)
        response = json.load(response)
        return response['access_token']
    except Exception:
        print ':('

def get_posts():
    try:
        token = get_token()
        request = urllib2.Request('https://api.producthunt.com/v1/posts', headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': ('Bearer %s' % token)
        })
        response = urllib2.urlopen(request)
        response = json.load(response)
        return map(format_posts, response['posts'])
    except Exception:
        print ':('

def format_posts(post):
    return (u'%s - %s| href=%s' % (post['name'], post['tagline'], post['discussion_url'])).encode('utf-8')

credentials = {
    'client_id': '92822f15f8f1dac5477cd3e8639d8153c70a5b976d2b55bad7cb117ff6d5bd72',
    'client_secret': 'fb5d3738f716b4c99cc3d798cd0a27401734edd20a8cf2efdc29a6c04dda344b',
    'grant_type': 'client_credentials'
}

print (u'\u2117 | size=18').encode('utf-8')
print '---'
print '\n'.join(get_posts())
print '---'
print 'Product Hunt - Today in Tech | href=https://www.producthunt.com/tech'
