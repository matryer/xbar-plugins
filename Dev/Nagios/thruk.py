#! /usr/bin/env PYTHONIOENCODING=utf8 PYTHONUNBUFFERED=1 /usr/local/bin/python3

# <bitbar.title>Thruk</bitbar.title>
# <bitbar.author>Brian Hartvigsen</bitbar.author>
# <bitbar.author.github>tresni</bitbar.author.github>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.image>https://monosnap.com/file/nFJPVlGURwBi7b8bU3ZFG8SVma7gEc.png</bitbar.image>

from urllib.parse import quote_plus
import sys

import requests

NAGIOS_HOST = ''
NAGIOS_USERNAME = ''
NAGIOS_PASSWORD = ''
ONLY_NEW = False

# Leave them all blank to get everything (can be seriously slow...)
checks = {
    'hosts': [],
    'hostgroups': [],
    'servicegroups': [],
    'services': []
}

# STOP EDITING HERE!

STATE_OK = 0
STATE_WARNING = 1
STATE_CRITICAL = 2
STATE_UNKNOWN = 3
STATE_PENDING = -1

params = {
    'view_mode': 'json',
    'style': 'detail',
}


def errOut(msg):
    print("💥 Thruk Error")
    print(msg, file=sys.stderr)  # noqa
    sys.exit(1)


def getData(key, value):
    p = {key: value}
    p.update(params)
    r = requests.get("%s/thruk/cgi-bin/status.cgi" % NAGIOS_HOST, params=p, auth=(NAGIOS_USERNAME, NAGIOS_PASSWORD))
    return r.json()

if not NAGIOS_HOST or not NAGIOS_USERNAME or not NAGIOS_PASSWORD:
    errOut('You must edit %s and enter your Nagios credentials' % sys.argv[0])

if not checks['hosts'] and not checks['hostgroups'] and not checks['services'] and not checks['servicegroups']:
    checks['hosts'].append("all")

hosts = {}
services = []
for group, values in checks.items():
    for v in values:
        try:
            jsondata = getData(group[:-1], v)
        except requests.exceptions.ConnectionError as e:
            errOut("Unable to connect to %s" % NAGIOS_HOST)
        for service in jsondata:
            dns_name = service['host_name'].split('.')
            dns_name.reverse()
            dns_name = '.'.join(dns_name)
            if dns_name not in hosts:
                hosts[dns_name] = {
                    'services': {},
                    'notifications': service['host_notifications_enabled'],
                    'offline': service['host_state'] == 1,
                    'disabled': service['host_checks_enabled'] == 0,
                    'name': service['host_name']
                }
            hosts[dns_name]['services'][service['display_name']] = service

for info in hosts.values():
    if not ONLY_NEW:
        info[STATE_OK] = len(list(filter(lambda x: info['services'][x]['state'] == 0, info['services'])))
        info[STATE_WARNING] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_WARNING,
                                              info['services'])))
        info[STATE_CRITICAL] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_CRITICAL,
                                               info['services'])))
        info[STATE_UNKNOWN] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_UNKNOWN,
                                              info['services'])))
    else:
        info[STATE_OK] = 0
        info[STATE_WARNING] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_WARNING and
                                              info['services'][x]['checks_enabled'] and
                                              info['services'][x]['notifications_enabled'] and
                                              not info['services'][x]['acknowledged'] and
                                              not info['disabled'] and
                                              not info['notifications'],
                                              info['services'])))
        info[STATE_CRITICAL] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_CRITICAL and
                                               info['services'][x]['checks_enabled'] and
                                               info['services'][x]['notifications_enabled'] and
                                               not info['services'][x]['acknowledged'] and
                                               not info['disabled'] and
                                               not info['notifications'],
                                               info['services'])))
        info[STATE_UNKNOWN] = len(list(filter(lambda x: info['services'][x]['state'] == STATE_UNKNOWN and
                                              info['services'][x]['checks_enabled'] and
                                              info['services'][x]['notifications_enabled'] and
                                              not info['services'][x]['acknowledged'] and
                                              not info['disabled'] and
                                              not info['notifications'],
                                              info['services'])))


print('%s%s%s%s%d%s %s%d%s %s%d%s ' % (
    '\033[1;32m' if not ONLY_NEW else '',
    sum([hosts[x][STATE_OK] for x in hosts]) if not ONLY_NEW else '',
    '\033[0m ' if not ONLY_NEW else '',

    '\033[1;33m',
    sum([hosts[x][STATE_WARNING] for x in hosts]),
    '\033[0m',

    '\033[1;31m',
    sum([hosts[x][STATE_CRITICAL] for x in hosts]),
    '\033[0m',

    '\033[1;30m',
    sum([hosts[x][STATE_UNKNOWN] for x in hosts]),
    '\033[0m',
))

print('---')

for k in sorted(hosts.keys()):
    info = hosts[k]
    host = info['name']
    print('%s%s%s %s%d %s%d %s%d %s%d%s %s%s| ansi=true' % (
        '\033[1;31m' if info['offline'] else '',
        host,
        '\033[0m' if info['offline'] else '',
        '\033[1;32m',
        info[STATE_OK],
        '\033[1;33m',
        info[STATE_WARNING],
        '\033[1;31m',
        info[STATE_CRITICAL],
        '\033[1;30m',
        info[STATE_UNKNOWN],
        '\033[0m',
        ':no_bell:' if not info['notifications'] else '',
        '❌' if info['disabled'] else ''
    ))
    print('-- View host in Nagios |  href="%s/thruk/#cgi-bin/status.cgi?host=%s"' % (NAGIOS_HOST, host))
    for s in info['services'].values():
        status = '✅'
        if s['state'] == STATE_WARNING:
            status = '⚠️'
        elif s['state'] == STATE_CRITICAL:
            status = '❗️'
        elif s['state'] == STATE_OK and not s['has_been_checked']:
            status = '❔'

        if not s['notifications_enabled']:
            status += ' :no_bell:'

        if s['acknowledged']:
            status += ' :+1:'

        print('-- %s %s | href="%s/thruk/#cgi-bin/extinfo.cgi?host=%s&service=%s&type=2"' % (
            status,
            s['display_name'],
            NAGIOS_HOST,
            host,
            quote_plus(s['display_name'])
        ))


print("refresh | refresh=true")
