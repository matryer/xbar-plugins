#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>GitHub Notifications</bitbar.title>
# <bitbar.version>v2.0.0</bitbar.version>
# <bitbar.author>Keith Cirkel, John Flesch</bitbar.author>
# <bitbar.author.github>flesch</bitbar.author.github>
# <bitbar.desc>GitHub (and GitHub:Enterprise) notifications in your menu bar!</bitbar.desc>
# <bitbar.image>https://cloud.githubusercontent.com/assets/13259/12300782/9f1f5ba8-b9e2-11e5-8e67-e59966aace9a.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
import os
import re

# GitHub.com
github_api_key = os.getenv( 'GITHUB_TOKEN', 'Enter your GitHub.com Personal Access Token here...' )

# GitHub:Enterprise (optional)
enterprise_api_key = os.getenv( 'GITHUB_ENTERPRISE_TOKEN', 'Enter your GitHub:Enterprise Personal Access Token here...' )
enterprise_api_url = os.getenv( 'GITHUB_ENTERPRISE_API', 'https://github.example.com/api/v3' )

active = '#4078C0'
inactive = '#7d7d7d'

def get_notifications( api_key, api_url = 'https://api.github.com' ):
    if len( api_key ) == 40:
        try:
            request = urllib2.Request( api_url + '/notifications', headers = { 'Authorization': 'token ' + api_key } )
            response = urllib2.urlopen( request )
            notifications = json.load( response )
            return map(format_notification, notifications)
        except Exception:
            return []
    else:
        return []

def get_release_url( release ):
    try:
        request = urllib2.Request( release, headers = { 'Authorization': 'token ' + github_api_key } )
        response = urllib2.urlopen( request )
        return json.load( response )['html_url']
    except Exception:
        return release

def format_notification( notification ):
    title = notification['subject']['title']
    repo = notification['repository']['full_name']
    url = notification['subject']['url']
    if 'releases/' in url:
        url = get_release_url( url )
    else:
        url = re.sub( 'api\.|api/v3/|repos/', '', notification['subject']['url'] )
        url = re.sub( '(pull|commit)s', ur'\1', url )
    return ( '%s: %s | length=90 refresh=true href=%s' % ( repo, title, url ) ).encode( 'utf-8' )

def plural( word, n ):
    return str(n) + ' ' + (word + 's' if n > 1 else word)

is_github_defined = len( github_api_key ) == 40
is_github_enterprise_defined = len( enterprise_api_key ) == 40

github_notifications = get_notifications( github_api_key ) if is_github_defined else []
enterprise_notifications = get_notifications( enterprise_api_key, enterprise_api_url ) if is_github_enterprise_defined else []
has_notifications = len( github_notifications ) + len( enterprise_notifications )

color = active if has_notifications else inactive

print ( u'\u25CF | color=' + color ).encode( 'utf-8' )
print '---'

if is_github_defined:
    if len( github_notifications ):
        print ( u'GitHub \u2014 %s | color=%s href=https://github.com/notifications' % ( plural( 'notification', len( github_notifications ) ), active ) ).encode( 'utf-8' )
        print '\n'.join( github_notifications )
    else:
        print ( u'GitHub \u2014 No new notifications | color=%s href=https://github.com' % inactive ).encode( 'utf-8' )

if is_github_enterprise_defined:
    if len( enterprise_notifications ):
        if is_github_defined:
            print '---'
        print ( u'GitHub:Enterprise \u2014 %s | color=%s href=%s/notifications' % ( plural( 'notification', len( enterprise_notifications ) ), active, re.sub( '/api/v3', '',  enterprise_api_url) ) ).encode( 'utf-8' )
        print '\n'.join( enterprise_notifications )
    else:
        print '---'
        print ( u'GitHub:Enterprise \u2014 No new notifications | color=%s' % inactive ).encode( 'utf-8' )
