#!/usr/bin/env PYTHONIOENCODING=UTF-8 /usr/local/bin/python3

# <xbar.title>GitLab Glance</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Kushal Pandya</xbar.author>
# <xbar.author.github>kushalpandya</xbar.author>
# <xbar.desc>Show review and authored MRs as well as assigned issues for current user for upcoming milestone.</xbar.desc>
# <xbar.dependencies>python3,requests</xbar.dependencies>
# <xbar.image>https://i.imgur.com/bFrtl6c.png</xbar.image>

# <xbar.var>string(GITLAB_API_TOKEN=""): Your GitLab API Access Token.</xbar.var>
# <xbar.var>string(GITLAB_URL="https://gitlab.com"): GitLab instance URL.</xbar.var>
# <xbar.var>string(GITLAB_GROUP=""): Complete path of group from which you want to fetch MRs and Issues.</xbar.var>
# <xbar.var>string(GITLAB_PROJECT=""): Complete path of project from which you want to fetch MRs and Issues (if you have set value for `GITLAB_GROUP`, this value will be ignored).</xbar.var>
# <xbar.var>string(GITLAB_USERNAME=""): Username for which you want to fetch MRs and Issues.</xbar.var>
# <xbar.var>select(DARK_MODE="auto"): Choose dark mode settings [auto, dark, light]</xbar.var>

import os
import urllib

# Third-party deps
import requests
from requests.structures import CaseInsensitiveDict


class Utils:
    @staticmethod
    def get_variable(key):
        return os.getenv(key)

    @staticmethod
    def get_gitlab_logo():
        return "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"

    @staticmethod
    def get_font_color():
        if (Utils.get_variable("DARK_MODE") == "dark"):
            return "white"
        elif (Utils.get_variable("DARK_MODE") == "light"):
            return "black"
        else:
            return None

    @staticmethod
    def get_sub_menu_item(issuable, author=False):
        iid = f"!{issuable.get('iid')}"
        title = issuable.get("title")
        author_name = issuable.get("author").get("name")
        href = f"href='{issuable.get('web_url')}'"
        length = "length=50"

        if (author == True):
            return f"--{iid} - {title} by {author_name}|{href}|{length}"
        else:
            return f"--{iid} - {title}|{href}|{length}"

    @staticmethod
    def print(string):
        color = Utils.get_font_color()
        if (color != None):
            print(f"{string}|color={color}")
        else:
            print(string)


class GitLabAPIHelper:
    def __init__(self, api_token, url, group, project, username):
        self.api_token = api_token
        self.url = url
        self.group = None if group == "" else group
        self.project = None if project == "" else project
        self.username = username

    def __get_request_headers(self):
        headers = CaseInsensitiveDict()
        headers["content-type"] = "application/json"
        headers["Authorization"] = f"Bearer {self.api_token}"

        return headers

    def __get_request_url(self):
        namespace_base_url = ""
        namespace = ""
        if (self.group != None):
            namespace_base_url = f"{self.url}/api/v4/groups"
            namespace = urllib.parse.quote(self.group)
        else:
            namespace_base_url = f"{self.url}/api/v4/projects"
            namespace = urllib.parse.quote(self.project)

        return f"{namespace_base_url}/{namespace}"

    def __get_response(self, endpoint, params):
        request_url = f"{self.__get_request_url()}/{endpoint}"
        headers = self.__get_request_headers()

        return requests.get(request_url, headers=headers, params=params).json()

    def __get_merge_requests(self, params):
        return self.__get_response("/merge_requests", params)

    def __get_issues(self, params):
        return self.__get_response("/issues", params)

    def get_review_merge_requests(self):
        return self.__get_merge_requests({"state": "opened", "milestone": "#upcoming", "reviewer_username": self.username})

    def get_authored_merge_requests(self):
        return self.__get_merge_requests({"state": "opened", "milestone": "#upcoming", "author_username": self.username})

    def get_assigned_issues(self):
        return self.__get_issues({"state": "opened", "milestone": "#upcoming", "assignee_username": self.username})


def main():
    api_token = Utils.get_variable('GITLAB_API_TOKEN')
    url = Utils.get_variable('GITLAB_URL')
    group = Utils.get_variable('GITLAB_GROUP')
    project = Utils.get_variable('GITLAB_PROJECT')
    username = Utils.get_variable('GITLAB_USERNAME')
    color = f"color={Utils.get_font_color()}"

    gitlab_helper = GitLabAPIHelper(api_token, url, group, project, username)
    review_mrs = gitlab_helper.get_review_merge_requests()
    author_mrs = gitlab_helper.get_authored_merge_requests()
    assigned_issues = gitlab_helper.get_assigned_issues()

    # Menu Button
    Utils.print(
        f" ⬇ {len(review_mrs)} ⬆ {len(author_mrs)} ◑ {len(assigned_issues)}|image={Utils.get_gitlab_logo()}")
    Utils.print("---")

    # Review Submenu
    Utils.print(f"👓 Review ({len(review_mrs)})")
    for merge_request in review_mrs:
        Utils.print(Utils.get_sub_menu_item(merge_request, True))

    # Authored Submenu
    Utils.print(f"📝 Authored ({len(author_mrs)})")
    for merge_request in author_mrs:
        Utils.print(Utils.get_sub_menu_item(merge_request))

    # Issues Submenu
    Utils.print(f"⁉️ Issues ({len(assigned_issues)})")
    for issue in assigned_issues:
        Utils.print(Utils.get_sub_menu_item(issue))

    # Options
    Utils.print("---")
    Utils.print(f"Refresh|refresh=true")


if __name__ == "__main__":
    main()
