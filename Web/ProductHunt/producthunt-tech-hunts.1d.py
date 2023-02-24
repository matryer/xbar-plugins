#!/usr/bin/env python3

# <xbar.title>Product Hunt - Today in Tech</xbar.title>
# <xbar.version>v1.1.0</xbar.version>
# <xbar.author>John Flesch</xbar.author>
# <xbar.author.github>flesch</xbar.author.github>
# <xbar.desc>Today's featured tech hunts on Product Hunt</xbar.desc>
# <xbar.image>https://cloud.githubusercontent.com/assets/13259/12370591/3039c57e-bbdc-11e5-9b42-e4ab9f6bf851.png</xbar.image>
# <xbar.dependencies>python</xbar.dependencies>
# <xbar.abouturl>https://www.producthunt.com/</xbar.abouturl>

import json
import urllib.request, urllib.error, urllib.parse

def get_token():
    data = json.dumps(credentials)
    try:
        request = urllib.request.Request('https://api.producthunt.com/v1/oauth/token', headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Content-Length': len(data.encode('utf-8'))
        })
        response = urllib.request.urlopen(request, data.encode('utf-8'))
        response = json.load(response)
        return response['access_token']
    except Exception:
        print(':(')

def get_posts():
    try:
        token = get_token()
        request = urllib.request.Request('https://api.producthunt.com/v1/posts', headers = {
            'Accept': 'application/json',
            'Content-Type': 'application/json',
            'Authorization': ('Bearer %s' % token)
        })
        response = urllib.request.urlopen(request)
        response = json.load(response)
        return list(map(format_posts, response['posts']))
    except Exception:
        print(':(')

def format_posts(post):
    return ('%s - %s| href=%s' % (post['name'], post['tagline'], post['discussion_url'])).encode('utf-8')

credentials = {
    'client_id': '92822f15f8f1dac5477cd3e8639d8153c70a5b976d2b55bad7cb117ff6d5bd72',
    'client_secret': 'fb5d3738f716b4c99cc3d798cd0a27401734edd20a8cf2efdc29a6c04dda344b',
    'grant_type': 'client_credentials'
}

print('\u2117 | size=18')
print('---')
print(b'\n'.join(get_posts()).decode('utf-8'))
print('---')
print('Product Hunt - Today in Tech | href=https://www.producthunt.com/tech')
