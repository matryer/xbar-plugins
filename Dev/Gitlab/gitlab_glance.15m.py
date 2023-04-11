#!/usr/bin/env python3

# <xbar.title>GitLab Glance</xbar.title>
# <xbar.version>v2.0</xbar.version>
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
import json
from urllib import parse
from urllib.request import Request, urlopen

GTLB_LOGO_COLOR = "iVBORw0KGgoAAAANSUhEUgAAABMAAAASCAYAAAC5DOVpAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAE6ADAAQAAAABAAAAEgAAAAAdD3kWAAADeklEQVQ4EWVTTWhcVRQ+5977fmYyTdRJYyto+pNm0sxGkFoRhIkKVZAGRAXtolmIm1LEZRdqBKmLQjfFgIKaTRe1uBDBhZvOUpCuNAWbSGNCEVONNqbNm/fuvcfvvsmkA7nw5p57zne/e853zhBhCRGHffm5o/t/f2HilWCH1fN3T93fnm/x5bFkZapx8rcXDw31sCoY7VZLh50jOfdYEn+32mqOhfONZjMKe//qYZPCtB6vpd8aSU+H+I3Xm1FJNtVu21utJx9SzNNWhES76QCYTFMk8mCFrO7sbXd9wifJCymWVwOieXUhL8nCQevOiZpRT2xYhxOXpfL168Us0Q4m4N64Sg4lDgLz0r8BK/T06tTk8RDrA/KpmJkyLwX8x1dPdEv9sJQuQHF9267Y+FjN8KFN621FqwopeXM7TnR7qtHwSv0SKTa5l6yqVXrf+vfNQPWivdfZb5QUmfesjFeJdps2jz8ejvU7d/JiK1aqgjvLplptll1EB889Gkfn/8xtDkdcOoksElkX4tCcUqfg7wpGddgMGwKLT5TSHcfTBnrz5ml5yxSO1N+IBgomMcwGJYxA43JutklKe8t7ckIeVErHLEOPCNnYvcZyZqLujf9LQb3sHtHGmqJsgwnyeSQSvl0LCWjEufqw0J4RT3EKoJUVQ5kWqfgOAElcIRke9byxxvTfGujxcnec+/iQojZEg/s81eqoEvn5ghiZdhR/sbAOop841eQd5bBpaJ9QfdSTjlFLX24hFlURO+BozzBiBYgcWYVxZVLfl6OBafmEck9GU4JsrMNwVAaFhg/6csfr5RqANnvhS6roTgdIcBnNkcv9XSM8t904BM+Mv42i5nQIOnGhRPSxVO7uOt7Fs4MoKzQETRKGCFqxcl7+EOdnormlH0oyxKEnSX527BkW9ZWJ1ISzuBb6VSNWRzAGocs3cc7wGXQRN6yVa6aIZvjzhRWZbZmdf0A4xJeWfvzHR8eKwl/RmA0eQa6TkHIAREMgaZJXdfg8FdbThejTm8+XRGfHEp5t250ygyiLcB65tNQJdv7R+Ht8mM6bhFN3H2VjFHSKsjK57X6Wd5MLi98E3K2ZA+nB+eUs2LtWyLDnLL6ceLaYH/9VrjRELo+LnW9c27p8dDTE5WvS0nqA7d3ZtQcNe6Ty2VNVEF7E90EPKLPNuGf37/8D0N+JCbFFvhgAAAAASUVORK5CYII="
ISSUE_ICON = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAABZklEQVQ4EZWTvy5EQRSHZ9aSaAQP4BkoSESl0NJoVGhWTS3ZrCDegl6hkEhoCFmVZCUeQLRWi0TC+H5z50zu5lrhJN+eOf9ndub6EMKoc64Fi9BPngm0vPdn5NfQX5bocRxirMEDfIAHSYAavMEMDMIUxffUKIcljTAe4RZHXyG+ApInWIVhJaO9Jkg0uSIkDCTnZ9IT6CPoEFuWr64fpJ6StW1hogYqtkYbrMdhF45hwXagIv0xgXNlZIPEdtAlfoA9Hb3OzVqDZBfnykZ1MZJcS0m3Kw2qNT2eF446hqcJHbj6b4NXirZB0tQ1/rWB/dlzFG5Bm+JTdpOvUR1/E3tcKpbsFKp4abaOr0uGOienabsNXeE50y/I0ZMOdgRLUF0sKjWRL7991ntymFiDIRx2TsVssmmLnTD1Jk2PTRW4hHW4hvLHhOn0sb2jJ2Ug+4XKDyxO3UzO+aR/Ul2cDabflacr8RsW0IYbyL182QAAAABJRU5ErkJggg=="
MERGE_ICON = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEKADAAQAAAABAAAAEAAAAAA0VXHyAAABqklEQVQ4EV2TvS9EURDF31sfIZuISiPR+AMUCh2NrbYgPgrZKNffokWjUUkkRKJSr1qQaEhEQVAISyKyVrDP78yd+/KY5Nwzd87cefdjXpJgWZbVwDE4AlWPlcQyYltg1f0SfmqCB+oEZHfg07wsm4kJzLc9tl+I/SlwSYIWq/KIJx948T2ff8FXQLtccs2KaJsfQJNBMABkrUDJl3O38zi8oyJpmkIchWFRHvYayMZpXyB9zePr8JD7J9Lxw1Fwll14hysu9hSKbBLf8Pg5/g3oj7oxgRdw6EldRS4mkqM7uwZ9itvZmPS6X8bX4o5EzvnDXNvUXXV0blgIW3cBSr41YJkWBTeMviguLkrWJzaQpC+qciyUf6G4wn3lCu1cY5sLQG/9CKYkwHkRfPuQNPAM2mDOCuBosawZyMbJWIRZvKeK63qplvtVfekM6AWGwQSQ7XoBdWd8kYYEbAyMArV9Q9X1nk3u4YHA/85THZ1Xplx16C14A0+grB2sAJneNnbjLKLdAzFrKFh/rOwe6N+R1ZQXi1wQOAXzHssv0ZJCntpeR1Yz1RX/BefN5na6MjKwAAAAAElFTkSuQmCC"
AUTHOR_ICON = "iVBORw0KGgoAAAANSUhEUgAAABEAAAARCAYAAAA7bUf6AAAAAXNSR0IArs4c6QAAAERlWElmTU0AKgAAAAgAAYdpAAQAAAABAAAAGgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAAAEaADAAQAAAABAAAAEQAAAACOk5MBAAABrElEQVQ4EY2TPUsDQRCGb5OLCmKnICjaCvEDFNJYiPoHLNJa+BMs/BMW/gULaxtr0WBio9go2B5+FUbRVmPM+bybm7BJEB14bmfmZt6b3WxclFmapgPmB2uKn4OGc05+RF2M32RdJDyCakwwirMHSyBz7cU/WzyH4J26Ms13+F6MVfUTsBLz2IVNuIcm9IoMkhsGiclKCKpnA1R7pfESOCf406ibg0eQ1ToNBAlUOokeh3d+MlYJPIPsCZYhD7FNUssSOVYXUJAm8SzUQfYCC13fIpFAFXyzvSTWeUmgCH0C5GZg2tfjJNAlQmwCmsC2oAnmg4/cEl+C88X2QitJuwdFwhMYg1dY4ye+UROmn3kEdLcGdJFCy1OgizRJ8hhMYD0TKGQC6vmED2j1iegtVoJxqIMmuNaE+LpHZuoVfdv5yioqrGW4QOAh2GJ4EbPSKArPRAXUp9rSG/6hqkxA/m9mIvZ/kFCLRo2pO/ItiG0C/yFyMvX4PjsTnbIJWmGDnERCMzHlrN47pyS24AzUGBYSdpkupA5XU07BAVtvSG0bZKvt5d/PfSp3VP0DgZgvS5iWT9cAAAAASUVORK5CYII="


class Utils:

    @staticmethod
    def sanitize(string):
        # Replace pipe with identical unicode character
        return string.replace("|", "⏐")

    @staticmethod
    def get_variable(key):
        return os.getenv(key)

    @staticmethod
    def get_font_color():
        if Utils.get_variable("DARK_MODE") == "dark":
            return "white"
        elif Utils.get_variable("DARK_MODE") == "light":
            return "black"
        else:
            return None

    @staticmethod
    def get_sub_menu_item(issuable, author=False):
        if "/merge_requests" in issuable.get("web_url"):
            iid = f"!{issuable.get('iid')}"
        else:
            iid = f"#{issuable.get('iid')}"
        title = Utils.sanitize(issuable.get("title"))
        author_name = Utils.sanitize(issuable.get("author").get("name"))
        href = f"href='{issuable.get('web_url')}'"
        length = "length=50"

        if author == True:
            return f"--{iid} - {title} by {author_name}|{href}|{length}"
        else:
            return f"--{iid} - {title}|{href}|{length}"

    @staticmethod
    def print(string):
        color = Utils.get_font_color()
        if color != None:
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
        if self.group != None:
            namespace_base_url = f"{self.url}/api/v4/groups"
            namespace = parse.quote(self.group, safe="")
        else:
            namespace_base_url = f"{self.url}/api/v4/projects"
            namespace = parse.quote(self.project, safe="")

        return f"{namespace_base_url}/{namespace}"

    def __get_response(self, endpoint, params):
        request_url = f"{self.__get_request_url()}{endpoint}"
        request = Request(f"{request_url}?{parse.urlencode(params)}")
        request.add_header("content-type", "application/json")
        request.add_header("Authorization", f"Bearer {self.api_token}")

        return json.load(urlopen(request))

    def __get_merge_requests(self, params):
        return self.__get_response("/merge_requests", params)

    def __get_issues(self, params):
        return self.__get_response("/issues", params)

    def get_review_merge_requests(self):
        return self.__get_merge_requests({
            "state": "opened",
            "milestone": "#upcoming",
            "reviewer_username": self.username,
        })

    def get_authored_merge_requests(self):
        return self.__get_merge_requests({
            "state": "opened",
            "milestone": "#upcoming",
            "author_username": self.username,
        })

    def get_assigned_issues(self):
        return self.__get_issues({
            "state": "opened",
            "milestone": "#upcoming",
            "assignee_username": self.username,
        })

    def get_assigned_issues_urllib(self):
        return self.__get_response_urllib(
            "issues",
            {
                "state": "opened",
                "milestone": "#upcoming",
                "assignee_username": self.username,
            },
        )


def main():
    api_token = Utils.get_variable("GITLAB_API_TOKEN")
    url = Utils.get_variable("GITLAB_URL")
    group = Utils.get_variable("GITLAB_GROUP")
    project = Utils.get_variable("GITLAB_PROJECT")
    username = Utils.get_variable("GITLAB_USERNAME")
    color = f"color={Utils.get_font_color()}"

    gitlab_helper = GitLabAPIHelper(api_token, url, group, project, username)
    review_mrs = gitlab_helper.get_review_merge_requests()
    author_mrs = gitlab_helper.get_authored_merge_requests()
    assigned_issues = gitlab_helper.get_assigned_issues()

    # Menu Button
    Utils.print(
        f" ⬇ {len(review_mrs)} ⬆ {len(author_mrs)} ◑ {len(assigned_issues)}|image={GTLB_LOGO_COLOR} "
    )
    Utils.print("---")

    # Review Submenu
    Utils.print(f"Review ({len(review_mrs)})|templateImage={MERGE_ICON}")
    for merge_request in review_mrs:
        Utils.print(Utils.get_sub_menu_item(merge_request, True))

    # Authored Submenu
    Utils.print(f"Authored ({len(author_mrs)})|templateImage={AUTHOR_ICON}")
    for merge_request in author_mrs:
        Utils.print(Utils.get_sub_menu_item(merge_request))

    # Issues Submenu
    Utils.print(f"Issues ({len(assigned_issues)})|templateImage={ISSUE_ICON}")
    for issue in assigned_issues:
        Utils.print(Utils.get_sub_menu_item(issue))

    # Options
    Utils.print("---")
    Utils.print(f"Refresh|refresh=true")


if __name__ == "__main__":
    main()
