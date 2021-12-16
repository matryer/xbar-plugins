#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <xbar.title>Gerrit Incoming Code Reviews</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Ryan Sydnor</xbar.author>
# <xbar.author.github>ryansydnor</xbar.author.github>
# <xbar.desc>Displays your incoming code reviews.</xbar.desc>
# <xbar.dependencies>python</xbar.dependencies>

import sys
import json
import subprocess

gerrit_user = 'my-gerrit-user'
gerrit_host = 'gerrit.example.com'
gerrit_port = '29418'


def query_gerrit():
    gerrit_query = ['ssh', '%s@%s' % (gerrit_user, gerrit_host), '-p', gerrit_port,
                    'gerrit', 'query', '--format=JSON', '--all-approvals',
                    'status:open', 'reviewer:"%s"' % gerrit_user]
    try:
        results = subprocess.check_output(
            gerrit_query, stderr=subprocess.STDOUT)
        return strip_unused_results(results)
    except:
        print 'Unable to query gerrit'
        print '---'
        print "Ensure you've uploaded your SSH key to gerrit"
        sys.exit(1)


def strip_unused_results(results):
    # last line is blank
    # second to last line is an aggregate
    results = results.split('\n')
    results = results[:-2] if len(results) > 2 else []
    return [json.loads(x) for x in results]


def is_self(result):
    return result.get('owner', {}).get('username', '') == gerrit_user


def filter_self(results):
    return [x for x in results if not is_self(x)]


def print_results(results):
    print '%s CRs' % len(results)
    print '---'
    for r in results:
        subj = r.get('subject')
        url = r.get('url')
        print '%s | href=%s' % (subj, url)


def main():
    all_incoming = filter_self(query_gerrit())
    print_results(all_incoming)


if __name__ == "__main__":
    main()
