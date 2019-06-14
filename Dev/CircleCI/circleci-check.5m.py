#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>CircleCI Check</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Florent Segouin</bitbar.author>
# <bitbar.author.github>fsegouin</bitbar.author.github>
# <bitbar.desc>This plugin displays the build status of repositories listed on CircleCI.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/Qvdgb39.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>

# Florent Segouin
# github.com/fsegouin

# Based on Travis Check by Chris Tomkins-Tinch
# github.com/tomkinsc

# version history
# 1.0
#   initial commit
# 1.1
#   - Update icons
#   - Sort builds
#   - Add running builds

from urllib import unquote
import requests

# You need to set your CIRCLECI_API_TOKEN with an API Token from CircleCI.
CIRCLECI_API_TOKEN = ''

CIRCLECI_API_ENDPOINT = 'https://circleci.com/api/v1/'

# ======================================

SYMBOLS = {
    'running': u' ▶',
    'success': u' ✓',
    'failed': u' ✗',
    'timedout': u' ⚠',
    'canceled': u' ⊝',
    'scheduled': u' ⋯',
    'no_tests': u' ',
}

COLORS = {
    'running': '#61D3E5',
    'success': '#39C988',
    'failed': '#EF5B58',
    'timedout': '#F3BA61',
    'canceled': '#898989',
    'scheduled': '#AC7DD3',
    'no_tests': 'black',
}

NO_SYMBOL = u' ❂'


def getOutcomeKey(build):
    return build['outcome']


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
        branches = project['branches']
        running_builds = []
        recent_builds = []
        output.append(u'{}/{} | href={}'.format(user_name, repo_name, repo_href))

        for branch_name, branch in sorted(branches.iteritems()):
            if 'running_builds' in branch and len(branch['running_builds']) > 0:
                branch['running_builds'][0]['branch_name'] = branch_name
                running_builds.append(branch['running_builds'][0])

            if 'recent_builds' in branch and len(branch['recent_builds']) > 0:
                branch['recent_builds'][0]['branch_name'] = branch_name
                recent_builds.append(branch['recent_builds'][0])

        for running_build in running_builds:
            status = running_build['status']
            if not status in ['not_running']:
                color = 'color={}'.format(COLORS[status]) if COLORS[status] else ''
                symbol = SYMBOLS.get(status, NO_SYMBOL)
                branch_href = 'href=https://circleci.com/gh/{}/{}/tree/{}'.format(user_name, repo_name, running_build['branch_name'])
                output_msg = u'- {} {}'.format(symbol, unquote(running_build['branch_name']))
                output.append(u'{} | {} {}'.format(output_msg, branch_href, color))


        for recent_build in sorted(recent_builds, key=getOutcomeKey):
            outcome = recent_build['outcome']
            if not outcome in ['no_tests']:
                color = 'color={}'.format(COLORS[outcome]) if COLORS[outcome] else ''
                symbol = SYMBOLS.get(outcome, NO_SYMBOL)
                branch_href = 'href=https://circleci.com/gh/{}/{}/tree/{}'.format(user_name, repo_name, recent_build['branch_name'])
                output_msg = u'- {} {}'.format(symbol, unquote(recent_build['branch_name']))
                output.append(u'{} | {} {}'.format(output_msg, branch_href, color))

        output.append('---')

    for line in output:
        print line.encode('utf-8')


if __name__ == '__main__':
    if len(CIRCLECI_API_TOKEN) == 0:
        raise ValueError("token can not be empty")

    updateStatuses(getRessource('projects'))
