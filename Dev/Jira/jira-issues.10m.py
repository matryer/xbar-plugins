#!/usr/local/bin/python3

# <bitbar.title>Jira issues</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Stefano Teodorani</bitbar.author>
# <bitbar.author.github>teopost</bitbar.author.github>
# <bitbar.desc>Show your jira issues</bitbar.desc>
# <bitbar.image>https://i.ibb.co/3SZ6D30/Schermata-2019-11-23-alle-19-26-24.png</bitbar.image>
# <bitbar.dependencies>python</bitbar.dependencies>
# <bitbar.abouturl>https://www.stefanoteodorani.it/</bitbar.abouturl>


import requests
import json
import base64

from urllib.parse import quote
from requests.packages.urllib3.exceptions import InsecureRequestWarning

class Jira:
    base_url = ""
    juser = ""
    jpwd = ""
    s = ""
    issue_count = 0
    issue_results = ""
    max_results = 50
    active_jql = ""

    def __init__(self, BaseUrl, UserName, Password):
        self.base_url = BaseUrl
        self.juser = UserName
        self.jpwd = Password

        self.s = requests.Session()

        requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

    def Query(self, jql):
        self.active_jql = jql
        #expansion = 'summary,issuetype,status,assignee,renderedFields,names,schema,operations,editmeta,changelog,versionedRepresentations'
        expansion = 'summary,issuetype,status,assignee'
        payload = {'jql' : jql, 'maxResults' : self.max_results, 'fields' : expansion}
        # renderedFields,names,schema,operations,editmeta,changelog,versionedRepresentations
        r = self.s.get(self.base_url + "/rest/api/2/search", params=payload, verify=False, auth=(self.juser, self.jpwd))
        parsed = json.loads(r.text)
        self.issue_count, self.issue_results = parsed['total'],parsed['issues']

    def PrintIssues(self, filter, assignee=''):
        # print gray title
        print(filter)
        for issue in self.issue_results:
            #print(issue)
            status = issue['fields']['status']['name']
            summary = issue['fields']['summary'].replace('|', '-')

            if filter.upper() == 'DEBUG':
                print('DEBUG: ' + status + '-' + summary)

            try:
                assignee = " ▸" + issue['fields']['assignee']['name']
            except:
                assignee = ""

            assignee = ""
            iconurl = issue['fields']['issuetype']['iconUrl']
            key = issue['key']

            line = key + ': ' + summary + assignee + ' | href=' + self.base_url + '/browse/' + key + ' image=' + self.render_icon_string(iconurl)
            if status.upper() != 'DEBUG':
                if status.upper() == filter.upper():
                    if assignee == '':
                        print(line)
                    elif assignee == assignee:
                        print(line)

    def render_icon_string(self, iconurl):
        if 'viewavatar' in iconurl:
            retvalue = iconurl + '&format=png'
            retvalue = retvalue.replace('size=medium', 'size=xsmall')
        else:
            retvalue = iconurl
        return str(base64.b64encode(self.s.get(retvalue).content))[2:-1]

def print_title(toprint):
    print(toprint)
    print('---')

def print_separator():
    print('---')

# ====
# MAIN
# ====
if __name__ == '__main__':

    QUERY_NAME = "Issues"

    JIRAURL = "<enter-jira-url>"
    USERNAME = "<put here usernema>"
    PASSWORD = "<put here password>"
    JQL = "resolution = Unresolved"

    Cloud = Jira(JIRAURL, USERNAME, PASSWORD)
    Cloud.Query(JQL)

    tot_count=str(Cloud.issue_count)

    if tot_count == 0:
        print_title(QUERY_NAME)
    else:
        print_title(QUERY_NAME + ": " + tot_count)

    Cloud.PrintIssues('Triage')
    Cloud.PrintIssues('Riaperta')
    Cloud.PrintIssues('Waiting for support')
    Cloud.PrintIssues('Waiting for customer')
    Cloud.PrintIssues('In corso')
    # Add here your preferrend jira workflow steps

    print_separator()

    print("▸ Go to Jira..." + ' | href=' + Cloud.base_url + '/issues/?jql=' + quote(Cloud.active_jql, ''))
