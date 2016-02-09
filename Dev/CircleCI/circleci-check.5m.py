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

# You need to set your CIRCLECI_API_TOKEN with an API Token from CircleCI.
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
    url = CIRCLECI_API_ENDPOINT + uri + '?circle-token=' + CIRCLECI_API_TOKEN
    headers = {'Accept': 'application/json'}
    r = requests.get(url, headers=headers)
    return r.json()


def getRessource(ressource_name):
    return request(ressource_name)


def updateStatuses(projects):
    output = []

    output.append('CircleCI')
    output.append('---')

    for project in projects:
        user_name = project['username']
        repo_name = project['reponame']
        repo_href = project['vcs_url']
        output.append(u'{}/{} | href={}'.format(user_name, repo_name, repo_href))
        branches = project['branches']

        for branch_name, branch in branches.iteritems():
            outcome = branch['recent_builds'][0]['outcome']
            color = 'color={}'.format(COLORS[outcome]) if COLORS[outcome] else ''
            symbol = SYMBOLS.get(outcome, NO_SYMBOL)
            branch_href = 'href=https://circleci.com/gh/{}/{}/tree/{}'.format(user_name, repo_name, branch_name)
            output_msg = u'- {} {}'.format(symbol, unquote(branch_name))
            output.append(u'{} | {} {}'.format(output_msg, branch_href, color))

        output.append('---')

    for line in output:
        print line.encode('utf-8')


if __name__ == '__main__':
    if len(CIRCLECI_API_TOKEN) == 0:
        raise ValueError("token can not be empty")

    updateStatuses(getRessource('projects'))
