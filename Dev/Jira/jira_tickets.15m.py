#!/usr/bin/env python
# -*- coding: utf-8 -*-

# <bitbar.title>Jira</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ryan Sydnor</bitbar.author>
# <bitbar.author.github>ryansydnor</bitbar.author.github>
# <bitbar.desc>Display your outstanding Jira tickets sorted by status.</bitbar.desc>
# <bitbar.dependencies>python,pip[jira]</bitbar.dependencies>

import sys
from collections import defaultdict

jira_url = 'https://jira.example.com'
jira_username = 'my-user-name'
jira_password = 's00p3rs3cr3t'

try:
    from jira import JIRA
    from jira import JIRAError
except ImportError:
    print "Dependency Error"
    print "---"
    print "Please run: 'pip install jira'"
    sys.exit(1)


def sort_jira_tickets(tickets):
    tickets_by_status = defaultdict(list)

    for ticket in tickets:
        tickets_by_status[ticket.fields.status.name].append(ticket)

    return tickets_by_status


def get_outstanding_tickets(user, password):
    jira_options = {'server': jira_url}
    try:
        jira = JIRA(jira_options, basic_auth=(user, password))
        tickets = jira.search_issues('assignee=%s AND status not in ("Done", "Resolved")' % user)
    except JIRAError:
        print '🐲'
        print '---'
        print 'Could not connect to Jira'
        sys.exit(1)
    return tickets


def print_tickets(tickets):
    print '🐲'
    print '---'
    for status, ticks in tickets.iteritems():
        print '%s' % status
        print '---'
        for ticket in ticks:
            print '%s|href=%s/browse/%s' % (ticket.fields.summary, jira_url, ticket.key)


def main():
    outstanding_tickets = get_outstanding_tickets(jira_username, jira_password)
    tickets_by_status = sort_jira_tickets(outstanding_tickets)
    print_tickets(tickets_by_status)


if __name__ == "__main__":
    main()
