#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>CircleCI Check</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Florent Segouin</bitbar.author>
# <bitbar.author.github>fsegouin</bitbar.author.github>
# <bitbar.desc>This plugin displays the build status of repositories listed on CircleCI.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

# Florent Segouin
# github.com/fsegouin

# Based on Travis Check by Chris Tomkins-Tinch
# github.com/tomkinsc

# version history
# 1.0
#   initial commit

from urllib import unquote
import requests

# You need to set your CIRCLECI_API_TOKEN to an API Token from CircleCI.
CIRCLECI_API_TOKEN = ''

CIRCLECI_API_ENDPOINT = 'https://circleci.com/api/v1/'

# ======================================

SYMBOLS = {
    'success': u'✔︎',
    'failed': u'✘',
    'timedout': u'⚠',
    'canceled': u' ⃠',
}
COLORS = {
    'success': 'green',
    'failed': 'red',
    'timedout': 'yellow',
    'canceled': 'grey',
}
NO_SYMBOL = u'❂'


def request(uri):
    if len(CIRCLECI_API_TOKEN) == 0:
        raise ValueError("token can not be empty")

    url = CIRCLECI_API_ENDPOINT + uri + '?circle-token=' + CIRCLECI_API_TOKEN
    headers = {'Accept': 'application/json'}
    r = requests.get(url, headers=headers)
    return r.json()


def get_projects():
    url = 'projects'
    projects = request(url)
    return projects


def update_statuses(projects):
    output = []

    output.append(u'CircleCI')
    output.append('---')

    for project in projects:
        username = project['username']
        reponame = project['reponame']
        repohref = project['vcs_url']
        output.append(u'{}/{} | href={}'.format(username, reponame, repohref))
        branches = project['branches']
        for branchname, branch in branches.iteritems():
            outcome = branch['recent_builds'][0]['outcome']
            color = 'color={}'.format(COLORS[outcome]) if COLORS[outcome] else ''
            symbol = SYMBOLS[outcome] or NO_SYMBOL
            branchhref = 'href=https://circleci.com/gh/{}/{}/tree/{}'.format(username, reponame, branchname)
            output_msg = u' - {} {}'.format(symbol, unquote(branchname))
            output.append(u'{} | {} {}'.format(output_msg, branchhref, color))
        output.append('---')

    for line in output:
        print line.encode('utf-8')


if __name__ == '__main__':
    update_statuses(get_projects())
