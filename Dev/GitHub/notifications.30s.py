#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>GitHub Notifications</bitbar.title>
# <bitbar.version>v3.0.0</bitbar.version>
# <bitbar.author>Keith Cirkel, John Flesch</bitbar.author>
# <bitbar.author.github>flesch</bitbar.author.github>
# <bitbar.desc>GitHub (and GitHub:Enterprise) notifications in your menu bar!</bitbar.desc>
# <bitbar.image>https://i.imgur.com/hW7dw9E.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

import json
import urllib2
import os
import sys
import re
from itertools import groupby

# GitHub.com
github_api_key = os.getenv( 'GITHUB_TOKEN', 'Enter your GitHub.com Personal Access Token here...' )

# GitHub:Enterprise (optional)
enterprise_api_key = os.getenv( 'GITHUB_ENTERPRISE_TOKEN', 'Enter your GitHub:Enterprise Personal Access Token here...' )
enterprise_api_url = os.getenv( 'GITHUB_ENTERPRISE_API', 'https://github.example.com/api/v3' )

active = '#4078C0'
inactive = '#7d7d7d'

# Utility Functions

def plural( word, n ):
    return str(n) + ' ' + (word + 's' if n > 1 else word)

def get_dict_subset( thedict, *keys ):
    return dict([ (key, thedict[key]) for key in keys if key in thedict ])

def print_bitbar_line( title, **kwargs ):
    print title + ' | ' + ( ' '.join( [ '{}={}'.format( k, v ) for k, v in kwargs.items() ] ) )

def make_github_request( url, method='GET', data=None, enterprise = False ):
    try:
        api_key = enterprise_api_key if enterprise else github_api_key
        headers = {
            'Authorization': 'token ' + api_key,
            'Accept': 'application/json',
        }
        if data is not None:
            data = json.dumps(data)
            headers['Content-Type'] = 'application/json'
            headers['Contnet-Length'] = len(data)
        request = urllib2.Request( url, headers=headers )
        request.get_method = lambda: method
        response = urllib2.urlopen( request, data )
        return json.load( response ) if response.headers.get('content-length', 0) > 0 else {}
    except Exception:
        return None

def get_notifications( enterprise ):
    url = '%s/notifications' % (enterprise_api_url if enterprise else 'https://api.github.com')
    return make_github_request( url, enterprise=enterprise ) or []

def print_notifications( notifications, enterprise=False ):
    notifications = sorted( notifications, key=lambda notification: notification['repository']['full_name'] )
    for repo, repo_notifications in groupby( notifications, key=lambda notification: notification['repository']['full_name'] ):
        if repo:
            repo_notifications = list( repo_notifications )
            print_bitbar_line( title=repo )
            print_bitbar_line(
                title='{title} - Mark {count} As Read'.format( title=repo, count=len( repo_notifications ) ),
                alternate='true',
                refresh='true',
                bash=__file__,
                terminal='false',
                param1='readrepo',
                param2=repo,
                param3='--enterprise' if enterprise else None
            )
            for notification in repo_notifications:
                formatted_notification = format_notification( notification )
                print_bitbar_line( refresh='true', **get_dict_subset( formatted_notification, 'title', 'href', 'image', 'templateImage' ) )
                print_bitbar_line(
                    refresh='true',
                    title='%s - Mark As Read' % formatted_notification['title'],
                    alternate='true',
                    bash=__file__,
                    terminal='false',
                    param1='readthread',
                    param2=formatted_notification['thread'],
                    param3='--enterprise' if enterprise else None,
                    **get_dict_subset( formatted_notification, 'image', 'templateImage' )
                )

def format_notification( notification ):
    type = notification['subject']['type']
    formatted = {
        'thread': notification['url'],
        'title': notification['subject']['title'].encode('utf-8'),
        'href': notification['subject']['url'],
        'image': 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAYAAAAmlE46AAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAALEwAACxMBAJqcGAAAA',
    }
    if len(formatted['title']) > 90:
        formatted['title'] = formatted['title'][:79] + '…'
    latest_comment_url = notification.get( 'subject', {} ).get( 'latest_comment_url', None )
    typejson = make_github_request( formatted['href'] )
    if latest_comment_url:
        formatted['href'] = ( make_github_request( latest_comment_url ) or {} ).get( 'html_url', formatted['href'] )
    # Try to hack a web-viewable URL if the last check failed
    if formatted['href']:
        formatted['href'] = re.sub( 'api\.|api/v3/|repos/', '', re.sub( '(pull|commit)s', ur'\1', formatted['href'] ) )
    if (type == 'PullRequest'):
        if typejson and typejson['merged']:
            formatted['image'] += 'SpJREFUKJG9kkFOwmAQhb+ZQiVx5xm4hIlncEF7jLZuWSjSeAJsvQUY4xkMHsCtcU9MXBmwJsy4EWgFEt34VpP55mX+eflhj9KoGO5jAK00LmOwoZiaYIPRbXaXRsVQRC6BvWZJ4uLJRI6DcKlUMsVl/G0CwIw3UR8V4+QKxFd9BbfDqiP6buo1sB5QjgTJ07i8aPTFgvNFa/7i7fYzaL+YpEN3zwGux4mY2QmAm6db783i0rO4bGyrh7OL66a0Bigm6d5gGkYz3brvV8a/SjeF/dPGJLrpmTMDXs/i4vTnQNYrInNm5szqvIVYbiJdCV1Z6ANwXze6em4i3SBcqi+CNVeAIFxq9dkR0+07HfHVz6rzlsLAK5keUCEu/R0hDD7C+SME6A7+Z30BqF2G+GPLjSUAAAAASUVORK5CYII='
        elif typejson and typejson['state'] == 'closed':
            formatted['image'] += 'Q9JREFUKJG9kjFOw0AQRd9sbAiiQOIMqWJfAInOPULANehTQIjFCbgHFS1eKhR6cDpEbyHRmljyDpWVTWJbSsOvRvvm72i+BjpkI2ZdDIBszNVzTG7HvGcx543JxmifT7KIj4FyUlaYcMjcKI8Id17Pj8JDknMvrD4zoriwRg4OMaKtU44FUhtz6z8aFW7KkC9X82mESbJghpICJDkiyimAwvV2EDG6uZMfThs3TeFYB8miP1XjFb0pdhp31cro/muijbjAUTjDtx1zttnwEnGJo8BR+DxQSAcwWv5i9vd4BZ58Yy2kgTIqq3VuAMoKEx4htCQrijaX5fMAYRoOmbMEJ0y2lhGmZcgbNdDGd9Uf3M1iNlKZZGMAAAAASUVORK5CYII='
        else:
            formatted['image'] += 'TJJREFUKJG9krFOAlEQRe/MLpqHnd/ATyBWsLWF+hcLPYXibvwCE/gLC2MNVrD7AbbGnpjYboBlro2QXWATabzVy5x338ydPKBC4awTVTEA8Huz9i1FI5gZBINh6+0lnHUiEXkAUGmWMGm/c7Fues4pLU9IPv+aAABm9i2qT6Pm+BECbuoqpnZWW4lmmRLG3ZdV9VyAOEyD+1JdVO4yqX+ua7UPofRHrUlEMgaA4cVYALkEACF6e/N2k4DdJCh1Ky7nENftyVACo9akcjElox3I9yfjsdoaVfFPHbtpcG2GOUy/wjS42r0QTjs3ZpibYV7kPmkxVuuGOKfIl1MAr0WjqMRcrBqec8oCVwDwnNOFnwugezkJ4+ZnFbkPeANanpwsAQr7+2m8Qab1FKcAeYgfqR/3P4pMOYR15QAAAABJRU5ErkJggg=='
        if typejson and typejson.get( 'user', {} ).get( 'login', None ):
            formatted['title'] += ' (by @{})'.format(typejson['user']['login'])
    elif (type == 'RepositoryInvitation'):
        formatted['image'] = 'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAKCAYAAACE2W/HAAAAAXNSR0IArs4c6QAAAM1JREFUKBWVkD0OQUEUhcdv/ASJn55SyxLoVBJq8tZjBRQsgkZiAQoqOiQ2oFc935nMvLxEXsRJPufOufe+kTHGmBeEf3Jg3i4t8IwOP6QZzeoi+3PFt1BRkKAq+Q4uEKbdUA+/wxm6LoubshPcoO8b9lp3GOMPGPomPoInqOcV/VUfyKfwhsChegJxfS3O6R5hALpFqFY2A69osUyygT3UQOpAWwVSpt4aShAt6lWXkIUk5WiswL5qiqIODdBz6+ZirM67cwFvulwfaH0AC7M1lHL62U4AAAAASUVORK5CYII=';
        formatted['templateImage'] = formatted.pop('image');
    elif (type == 'Issue'):
        if typejson and typejson['state'] == 'closed':
            formatted['image'] += 'YpJREFUKJGdkj9I23EQxT93SZrJQYNQ6Bo65BtQcHRpSeyUuQqNWx2ti7qJRpqhUxGX0lGhlXQ0uJhfU7cOLm3+QKCzoIjoJpp8z0HT/BpJBd90HO/dvTsePBLS36iM8Uw6vANyQNJ7RJU/wG7nmo1XLY7uCStp8uL5BJwgbJtRU8E8jImR98qoGHPZBjsSFhlsCRQjCQovD2iHhx5OEDu/ZF2E5Y4wLn/tXdMy5WO2zkqXHKQxgEy95+y7Y1KMXwogbeaBk0iCwqBn7DteAJym+NmBnSiAF3IK2/32wiIVqoGjYE1GRMlGARSSZtQGbZtq8CNwFBBWDUxgJjqI3I9Mg7XAgRoXmSYlACqOWuAG39ePimNB7+oyxuzhBLEwIUhj3c92sZckLsaSAvg2m14ZPb98eGs8ThFlqBcAx4wJX8T4cGasvm5yFRaUUjwZUd4bLKow/W/kbsWfgTMxvorxmwhtjHEPbxSGEd5many7F/JqiqdemceT88rzu3ZLoWywma1z/NA5/8UNNkSJCdaYQF4AAAAASUVORK5CYII='
        else:
            formatted['image'] += 'ZxJREFUKJGdkjFoU2EUhb97k9jNRzEFoWvJVHXoZCqIaQaH7JaUbtpi2zc4ORWJYKGTYJLBroFaiGPo0hBwyAOhU51Cd8EOOid53uugL4SnkOK3/dx7OPccfvhPZPqxHZUWc0gIUjFs6c/CJdDBtN580P36lzCM1jZ+mr9HuFKkJfgXABO9i/mmqOTd7VlztXcyEYbR2oabtFx5kx+Oa7VHn+LpS7bOV3K5UfBaTF+6WLW52juR7ai0mDEGovq2WezuJ8t7UdkBGsXu5KqdfvlQhOeeGRc0h4QIV/nhuDarkIVgft+xHxJnQwWpKNJKn/cvasvtkcCxuFTUsKWkiOsgrhcoBb2uIE1W4NKRO8DH6cF0KSnLewYDBTqGb26dr+Sm53tR2ZNmE8LTx3O4V9XpKKZ1FV24MZyf2aoH8YE5garXBWC3X1oX12NXO8zfvPWqttwepZ08iA9wXqDypFE8a09y7PZL64geCXx37IO4XiSZcK/+dso8bRTP2pD65DufH96WOBuKSwWlAGD4QJ2Oqtff3e99mxVnJr8AXSGi02ni0+YAAAAASUVORK5CYII='
    elif (type == 'Commit'):
        formatted['image'] += 'HhJREFUKJHl0LEKwkAQBNCH3yIaf05S+VUqmh8ykFoUYn8WbnEc8a7XgYVlmNkdhv/EDgNmvHDFtmXq8ETCiFvsD2xqxksI9xnXB3cqxamYceHgVOpWC6JUi/QNQxj7jDsEd6wZ83KmLOId69bXzqekOeas0eiv4g3q4SY7NY1R2gAAAABJRU5ErkJggg=='
        formatted['templateImage'] = formatted.pop('image')
    elif (type == 'Release'):
        formatted['image'] += 'JdJREFUKJGl0DsKwkAUBdDTRgvFHbgmNyLY+QWzKxM/kK2kSKc70MIIQ0ziqBceA/dxinn8mSkKVMGUmH+CBWaNboQjdn2wqt97Pa8kNd5+C0O86YNdSZC34RLjCJxhHZYLXDCIxKuwTHGOwBNcm2WKUw9OcMCybZl6XjHpQOs30cB5gKNQiDPPP0WjV/a4aVwxNsNfUGce7P8k4XgVPSYAAAAASUVORK5CYII='
        formatted['templateImage'] = formatted.pop('image')
    return formatted

if len(sys.argv) > 1:
    command = sys.argv[1]
    args = sys.argv[2:]
    enterprise=False
    if ('--enterprise' in args):
        enterprise=True
        args.remove( '--enterprise' )
    if command == 'readrepo':
        url = '%s/repos/%s/notifications' % (enterprise_api_url if enterprise else 'https://api.github.com', args[0])
        print 'Marking %s as read' % url
        make_github_request( url=url, method='PUT', data={}, enterprise=enterprise )
    elif command == 'readthread':
        url = args[0]
        print 'Marking %s as read' % url
        make_github_request( url=url, method='PATCH', data={}, enterprise=enterprise )

else:
    is_github_defined = len( github_api_key ) == 40
    is_github_enterprise_defined = len( enterprise_api_key ) == 40
    github_notifications = get_notifications( enterprise=False ) if is_github_defined else []
    enterprise_notifications = get_notifications( enterprise=True ) if is_github_enterprise_defined else []
    has_notifications = len( github_notifications ) + len( enterprise_notifications )
    color = active if has_notifications else inactive

    if (has_notifications):
        print_bitbar_line(
            title=u'\u25CF'.encode( 'utf-8' ),
            color=color
        )
        print '---'
    else:
        print ''
        exit(0)

    print_bitbar_line( title='Refresh', refresh='true' )

    if is_github_defined:
        if len( github_notifications ):
            print_bitbar_line(
                title=( u'GitHub \u2014 %s' % plural( 'notification', len( github_notifications ) ) ).encode( 'utf-8' ),
                color=active,
                href='https://github.com/notifications',
            )
            print_notifications( github_notifications )
        else:
            print_bitbar_line(
                title=u'GitHub \u2014 No new notifications'.encode( 'utf-8' ),
                color=inactive,
                href='https://github.com',
            )

    if is_github_enterprise_defined:
        if len( enterprise_notifications ):
            if is_github_defined:
                print '---'
            print_bitbar_line(
                title=( u'GitHub:Enterprise \u2014 %s' % plural( 'notification', len( enterprise_notifications ) ) ).encode( 'utf-8' ),
                color=active,
                href='%s/notifications' % re.sub( '/api/v3', '',  enterprise_api_url ),
            )
            print_notifications( enterprise_notifications, enterprise=True )
        else:
            print '---'
            print_bitbar_line(
                title=u'GitHub:Enterprise \u2014 No new notifications',
                color=inactive,
            )
