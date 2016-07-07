#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Buildbot 9 Status</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ryan Sydnor</bitbar.author>
# <bitbar.author.github>ryansydnor</bitbar.author.github>
# <bitbar.desc>Displays most recent build status on configurable list of builders.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

import os
import sys
import json
import urllib2
from urllib2 import URLError, HTTPError

bitbar_header = ['BB', '---']
buildbot_url = 'http://nine.buildbot.net'
builders = ['db-postgres', 'py27-tw1020', 'db-mysql']


def parse_buildbot_status(url, status):
    return {
        'success': status['results'] == 0,
        'url': url + '#/builders/' + str(status['builderid']) + '/builds/' + str(status['number'])
    }


def get_buildbot_status(url, builder):
    builders_url = url + 'api/v2/builders'
    most_recent_build_url = builders_url + '/%s/builds?complete=true&order=-complete_at&limit=1'
    try:
        builder_id_results = json.loads(urllib2.urlopen(builders_url).read()).get('builders', None)
        builder_id = (b['builderid'] for b in builder_id_results if b["name"] == builder).next()
        builds = json.loads(urllib2.urlopen(most_recent_build_url % builder_id).read())
        return builds['builds'][0]
    except (URLError, HTTPError):
        bitbar_header.append("Could not connect to buildbot.")
        print '\n'.join(bitbar_header)
        sys.exit(0)
    except IndexError:
        bitbar_header.append("No recent builds.")
        print '\n'.join(bitbar_header)
        sys.exit(0)


def main():
    # add trailing slash if it doesn't already exist
    url = os.path.join(buildbot_url, '')
    for builder in builders:
        status = get_buildbot_status(url, builder)
        result = parse_buildbot_status(url, status)
        bitbar_header.append("%s | color=%s href=%s" % (builder, 'green' if result['success'] else 'red', result['url']))
    print '\n'.join(bitbar_header)


main()
