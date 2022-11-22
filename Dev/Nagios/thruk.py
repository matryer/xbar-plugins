#! /usr/bin/env PYTHONIOENCODING=utf8 PYTHONUNBUFFERED=1 /usr/local/bin/python3

# <xbar.title>Thruk</xbar.title>
# <xbar.author>Brian Hartvigsen</xbar.author>
# <xbar.author.github>tresni</xbar.author.github>
# <xbar.version>1.0</xbar.version>
# <xbar.image>PCFET0NUWVBFIGh0bWw+PGh0bWwgbGFuZz0iZW4iPjxoZWFkPjxtZXRhIGNoYXJzZXQ9InV0Zi04Ij48dGl0bGU+TW9ub3NuYXA8L3RpdGxlPjxtZXRhIG5hbWU9ImRlc2NyaXB0aW9uIiBjb250ZW50PSJNb25vc25hcCBzY3JlZW5zaG90IHRvb2wgZm9yIE1hYyBhbmQgUEMgd2l0aCBvd24gY2xvdWQgc3RvcmFnZS4gVGFrZSBzY3JlZW5zaG90cywgcmVjb3JkIHZpZGVvcyBhbmQgdXBsb2FkIGZpbGVzIGRpcmVjdGx5IHRvIHRoZSBjbG91ZC4gSnVzdCBpbiBvbmUgY2xpY2suIiAvPjxtZXRhIG5hbWU9InZpZXdwb3J0IiBjb250ZW50PSJ3aWR0aD1kZXZpY2Utd2lkdGgsaW5pdGlhbC1zY2FsZT0xLHNocmluay10by1maXQ9bm8iPjxtZXRhIHByb3BlcnR5PSJvZzp0aXRsZSIgY29udGVudD0ibW9ub3NuYXAuY29tIj48bWV0YSBwcm9wZXJ0eT0ib2c6ZGVzY3JpcHRpb24iIGNvbnRlbnQ9Ik1vbm9zbmFwIHNjcmVlbnNob3QgdG9vbCBmb3IgTWFjIGFuZCBQQyB3aXRoIG93biBjbG91ZCBzdG9yYWdlLiBUYWtlIHNjcmVlbnNob3RzLCByZWNvcmQgdmlkZW9zIGFuZCB1cGxvYWQgZmlsZXMgZGlyZWN0bHkgdG8gdGhlIGNsb3VkLiBKdXN0IGluIG9uZSBjbGljay4iPjxtZXRhIHByb3BlcnR5PSJvZzp1cmwiIGNvbnRlbnQ9Imh0dHBzOi8vbW9ub3NuYXAuY29tIj48bWV0YSBwcm9wZXJ0eT0ib2c6aW1hZ2UiIGNvbnRlbnQ9Imh0dHBzOi8vbW9ub3NuYXAuY29tL2xvZ28ucG5nIj48bWV0YSBwcm9wZXJ0eT0ib2c6dHlwZSIgY29udGVudD0id2Vic2l0ZSIgLz48bGluayByZWw9InByZWNvbm5lY3QiIGhyZWY9Imh0dHBzOi8vYXBpLm1vbm9zbmFwLmNvbSI+PGxpbmsgcmVsPSJwcmVjb25uZWN0IiBocmVmPSJodHRwczovL3d3dy5nb29nbGUtYW5hbHl0aWNzLmNvbSI+PGxpbmsgcmVsPSJwcmVjb25uZWN0IiBocmVmPSJodHRwczovL2hjYXB0Y2hhLmNvbSI+PGxpbmsgcmVsPSJwcmVjb25uZWN0IiBocmVmPSJodHRwczovL25ld2Fzc2V0cy5oY2FwdGNoYS5jb20iPjxsaW5rIHJlbD0icHJlY29ubmVjdCIgaHJlZj0iaHR0cHM6Ly9mb250cy5nb29nbGVhcGlzLmNvbSI+PGxpbmsgcmVsPSJwcmVjb25uZWN0IiBocmVmPSJodHRwczovL2ZvbnRzLmdzdGF0aWMuY29tIiBjcm9zc29yaWdpbj48bGluayByZWw9ImFwcGxlLXRvdWNoLWljb24iIHNpemVzPSIxODB4MTgwIiBocmVmPSIvYXBwbGUtdG91Y2gtaWNvbi5wbmciPjxsaW5rIHJlbD0iaWNvbiIgdHlwZT0iaW1hZ2UvcG5nIiBzaXplcz0iMzJ4MzIiIGhyZWY9Ii9mYXZpY29uLTMyeDMyLnBuZyI+PGxpbmsgcmVsPSJpY29uIiB0eXBlPSJpbWFnZS9wbmciIHNpemVzPSIxNngxNiIgaHJlZj0iL2Zhdmljb24tMTZ4MTYucG5nIj48bGluayByZWw9Im1hbmlmZXN0IiBocmVmPSIvbWFuaWZlc3QuanNvbj92PTIiPjxsaW5rIHJlbD0ibWFzay1pY29uIiBocmVmPSIvc2FmYXJpLXBpbm5lZC10YWIuc3ZnIiBjb2xvcj0iIzViYmFkNSI+PGxpbmsgcmVsPSJzaG9ydGN1dCBpY29uIiBocmVmPSIvZmF2aWNvbi5pY28iPjxtZXRhIG5hbWU9Im1zYXBwbGljYXRpb24tVGlsZUNvbG9yIiBjb250ZW50PSIjZmZmZmZmIj48bWV0YSBuYW1lPSJ0aGVtZS1jb2xvciIgY29udGVudD0iIzAwMDAwMCI+PHNjcmlwdCB0eXBlPSJhcHBsaWNhdGlvbi9sZCtqc29uIj57CiAgICAgICAgIkBjb250ZXh0IjogImh0dHBzOi8vc2NoZW1hLm9yZyIsCiAgICAgICAgIkBncmFwaCI6IFsKICAgICAgICAgICAgewogICAgICAgICAgICAgICAgIkB0eXBlIjogIk9yZ2FuaXphdGlvbiIsCiAgICAgICAgICAgICAgICAiQGlkIjogImh0dHBzOi8vbW9ub3NuYXAuY29tLyIsCiAgICAgICAgICAgICAgICAibmFtZSI6ICJNb25vc25hcCIsCiAgICAgICAgICAgICAgICAidXJsIjogImh0dHBzOi8vbW9ub3NuYXAuY29tLyIsCiAgICAgICAgICAgICAgICAibG9nbyI6IHsKICAgICAgICAgICAgICAgICAgICAiQHR5cGUiOiAiSW1hZ2VPYmplY3QiLAogICAgICAgICAgICAgICAgICAgICJAaWQiOiAiaHR0cHM6Ly9tb25vc25hcC5jb20vI2xvZ28iLAogICAgICAgICAgICAgICAgICAgICJpbkxhbmd1YWdlIjogImVuLVVTIiwKICAgICAgICAgICAgICAgICAgICAidXJsIjogImh0dHBzOi8vbW9ub3NuYXAuY29tL2xvZ28ucG5nIiwKICAgICAgICAgICAgICAgICAgICAid2lkdGgiOiAyNTYsCiAgICAgICAgICAgICAgICAgICAgImhlaWdodCI6IDI1NiwKICAgICAgICAgICAgICAgICAgICAiY2FwdGlvbiI6ICJNb25vc25hcCIKICAgICAgICAgICAgICAgIH0sCiAgICAgICAgICAgICAgICAiaW1hZ2UiOiB7CiAgICAgICAgICAgICAgICAgICAgIkBpZCI6ICJodHRwczovL21vbm9zbmFwLmNvbS8jbG9nbyIKICAgICAgICAgICAgICAgIH0KICAgICAgICAgICAgfQogICAgICAgIF0KICAgIH08L3NjcmlwdD48c2NyaXB0IGRlZmVyPSJkZWZlciIgc3JjPSIvc3RhdGljL2pzL21haW4uZDBlZmUxNTguanMiPjwvc2NyaXB0PjxsaW5rIGhyZWY9Ii9zdGF0aWMvY3NzL21haW4uM2U1YjEwNTAuY3NzIiByZWw9InN0eWxlc2hlZXQiPjwvaGVhZD48Ym9keSBzdHlsZT0iYmFja2dyb3VuZC1jb2xvcjojMWExYTFjIj48bm9zY3JpcHQgc3R5bGU9ImNvbG9yOiNmMGYwZjAiPllvdSBuZWVkIHRvIGVuYWJsZSBKYXZhU2NyaXB0IHRvIHJ1biB0aGlzIGFwcC48L25vc2NyaXB0PjxkaXYgaWQ9InJvb3QiPjxuYXYgc3R5bGU9InZpc2liaWxpdHk6aGlkZGVuIiBpdGVtc2NvcGUgaXRlbXR5cGU9Imh0dHA6Ly9zY2hlbWEub3JnL1dlYlNpdGUiPjxtZXRhIGl0ZW1wcm9wPSJ1cmwiIGNvbnRlbnQ9Imh0dHBzOi8vbW9ub3NuYXAuY29tLyIgLz48dWw+PGxpIGl0ZW1zY29wZT0iaXRlbXNjb3BlIiBpdGVtdHlwZT0iaHR0cHM6Ly93d3cuc2NoZW1hLm9yZy9TaXRlTmF2aWdhdGlvbkVsZW1lbnQiPjxhIGl0ZW1wcm9wPSJ1cmwiIGNsYXNzPSJoZWFkbGluayIgaHJlZj0iL2Rvd25sb2FkIj5Eb3dubG9hZDwvYT48bWV0YSBpdGVtcHJvcD0iZGVzY3JpcHRpb24iIGNvbnRlbnQ9IkRvd25sb2FkIE1vbm9zbmFwIGZvciBNYWMsIFdpbmRvd3MsIGFuZCBDaHJvbWUgdG8gY2FwdHVyZSBldmVyeXRoaW5nLCBhbnl3aGVyZS4iIC8+PC9saT48bGkgaXRlbXNjb3BlPSJpdGVtc2NvcGUiIGl0ZW10eXBlPSJodHRwczovL3d3dy5zY2hlbWEub3JnL1NpdGVOYXZpZ2F0aW9uRWxlbWVudCI+PGEgaXRlbXByb3A9InVybCIgY2xhc3M9ImhlYWRsaW5rIiBocmVmPSIvcGF5Ij5QcmljaW5nPC9hPjxtZXRhIGl0ZW1wcm9wPSJkZXNjcmlwdGlvbiIgY29udGVudD0iTW9ub3NuYXAgcHJpY2luZyBwbGFucyBmb3IgaW5kaXZpZHVhbCB1c2VycyBhbmQgYnVzaW5lc3NlcyBvZiBhbGwgc2l6ZXMuIENob29zZSBhIHBsYW4gdGhhdOKAmXMgcmlnaHQgZm9yIHlvdSEiIC8+PC9saT48bGkgaXRlbXNjb3BlPSJpdGVtc2NvcGUiIGl0ZW10eXBlPSJodHRwczovL3d3dy5zY2hlbWEub3JnL1NpdGVOYXZpZ2F0aW9uRWxlbWVudCI+PGEgaXRlbXByb3A9InVybCIgY2xhc3M9ImhlYWRsaW5rIiBocmVmPSIvI2xvZ2luIj5TaWduIEluPC9hPjxtZXRhIGl0ZW1wcm9wPSJkZXNjcmlwdGlvbiIgY29udGVudD0iU2lnbiBpbiB0byBNb25vc25hcCB0byBhY2Nlc3MgZWFzeS10by11c2Ugc2NyZWVuc2hvdCBhbmQgdmlkZW8gcmVjb3JkaW5nIHNlcnZpY2UuIiAvPjwvbGk+PGxpIGl0ZW1zY29wZT0iaXRlbXNjb3BlIiBpdGVtdHlwZT0iaHR0cHM6Ly93d3cuc2NoZW1hLm9yZy9TaXRlTmF2aWdhdGlvbkVsZW1lbnQiPjxhIGl0ZW1wcm9wPSJ1cmwiIGNsYXNzPSJoZWFkbGluayIgaHJlZj0iL2VudGVycHJpc2UiPkVudGVycHJpc2U8L2E+PG1ldGEgaXRlbXByb3A9ImRlc2NyaXB0aW9uIiBjb250ZW50PSJNb25vc25hcCBwcm92aWRlcyBzZWN1cmUgY29udGVudCBtYW5hZ2VtZW50IGFuZCBzY2FsYWJsZSBzb2x1dGlvbnMgZm9yIHlvdXIgYnVzaW5lc3MuIiAvPjwvbGk+PC91bD48L25hdj48L2Rpdj48L2JvZHk+PC9odG1sPg==</xbar.image>

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
        except requests.exceptions.ConnectionError:
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
