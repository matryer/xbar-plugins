#!/usr/bin/env PYTHONIOENCODING=UTF-8 /path/to/the/python3
# -*- coding: utf-8 -*-

# <bitbar.title>Github PR buddy</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Luis Almeida Santos</bitbar.author>
# <bitbar.author.github>luis-santos-teampicnic</bitbar.author.github>
# <bitbar.desc>This plugin displays relevant Pull Requests in Github repositories.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>

# Luis Almeida Santos
# github.com/luis-santos-teampicnic

# version history
# 2.0
#   Support for python3
# 1.0
#   Initial commit

import base64
import json
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime

# --------------------------------------------------------------------------------
# Configure the plugin here
# --------------------------------------------------------------------------------

# Set your github username here
USERNAME = "<your-username>"

# Create a Github token and set it here
#  https://github.com/settings/tokens/new
#   Add, at least, the repo scope
TOKEN = "ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# list all the repositories you want to check
REPOS_TO_CHECK = [
    "PicnicSupermarket/jolo",
    "PicnicSupermarket/hiring-experience",
    "PicnicSupermarket/oss-parent",
    "PicnicSupermarket/nepsnowplow",
    "PicnicSupermarket/diepvries",
    "PicnicSupermarket/reactive-support",
    "PicnicSupermarket/employer-statement-generator",
]

# The number of approvals for a PR to be able to be merged
MINIMUM_APPROVALS = 2

# Show the PR number for each line
SHOW_PR_NUMBER = False

# --------------------------------------------------------------------------------
# There should be no changes bellow this line
# --------------------------------------------------------------------------------

BASE_URL = "https://api.github.com"
AUTHORIZATION = "Basic " + base64.b64encode(
    f"{USERNAME}:{TOKEN}".encode("utf-8")
).decode("utf-8")

USER_AGENT = f"{USERNAME} - prbuddy - xbar"


def get(uri):
    request = urllib.request.Request(
        BASE_URL + uri,
        headers={
            "User-Agent": USER_AGENT,
            "Content-Type": "application/json",
            "Authorization": AUTHORIZATION,
        },
    )

    try:
        with urllib.request.urlopen(request) as response:
            return json.load(response)
    except Exception as e:
        print("🛑  HTTP GET Error!")
        print("---")
        print(
            f"Check if you have the correct access code in the plugin - could also be an API/HTTP error [{BASE_URL}{uri}]"
        )
        print(e)
        sys.exit(1)


def get_pr_reviews(repo, pr_number):
    return get(f"/repos/{repo}/pulls/{str(pr_number)}/reviews")


def get_status_from_reviews(data):
    reviews = {}
    for review in data:
        state = review["state"]
        if state in reviews:
            reviews[state] = reviews[state] + 1
        else:
            reviews[state] = 1
    return reviews


def get_pull_requests(repo):
    pull_requests = get(f"/repos/{repo}/pulls")
    pull_requests.sort(key=lambda x: (x["user"]["login"], x["created_at"]))
    return pull_requests


def produce_pr_output(repo, pr):
    pr_number = pr["number"]
    reviews = get_pr_reviews(repo, pr_number)

    labels = [label["name"] for label in pr["labels"]]

    requested_reviewers = [label["login"] for label in pr["requested_reviewers"]]
    requested_teams = [label["name"] for label in pr["requested_teams"]]
    reviewers = requested_reviewers + requested_teams

    own_reviews = [
        r["state"] for r in [re for re in reviews if re["user"]["login"] == USERNAME]
    ]

    reviews_status = get_status_from_reviews(reviews)
    review_status_line = "{0:02d}✅ {1:02d}🛑 {2:02d}🎙 {3:02d}⏰".format(
        reviews_status["APPROVED"] if "APPROVED" in reviews_status else 0,
        reviews_status["CHANGES_REQUESTED"]
        if "CHANGES_REQUESTED" in reviews_status
        else 0,
        reviews_status["COMMENTED"] if "COMMENTED" in reviews_status else 0,
        reviews_status["PENDING"] if "PENDING" in reviews_status else 0,
    )

    approved_reviews_count = (
        reviews_status["APPROVED"] if "APPROVED" in reviews_status else 0
    )
    own_pr = pr["user"]["login"] == USERNAME

    if "APPROVED" in own_reviews:
        own_status = "✅"
    elif "CHANGES_REQUESTED" in own_reviews:
        own_status = "🛑"
    elif "COMMENTED" in own_reviews:
        own_status = "🎙"
    elif "PENDING" in own_reviews:
        own_status = "⏰"
    else:
        own_status = "  "

    created_at = datetime.strptime(pr["created_at"], "%Y-%m-%dT%H:%M:%SZ")
    days_old = (datetime.now() - created_at).days

    output_msg = "{} {}{}[{}] {}{} [{} days old]{}".format(
        "*" if own_pr else "-",
        own_status,
        " (" + str(pr_number) + ") " if SHOW_PR_NUMBER else "",
        review_status_line,
        pr["title"],
        " {" + ",".join(labels) + "}" if len(labels) > 0 else "",
        days_old,
        " -> " + ",".join(reviewers) if len(reviewers) > 0 else "",
    )

    href = pr["html_url"]

    if approved_reviews_count >= MINIMUM_APPROVALS:
        color = "color=green"
    elif approved_reviews_count >= MINIMUM_APPROVALS / 2:
        color = "color=orange"
    elif approved_reviews_count > 0:
        color = "color=yellow"
    else:
        color = ""

    return f"{output_msg} | href={href} {color} font=JetBrainsMono-Regular"


def get_all_prs(repos):
    output = []
    output.append("---")
    pr_count = 0

    for repo in repos:
        pull_requests = get_pull_requests(repo)

        # repository header
        output.append("---")
        output.append(
            f"{repo} ({len(pull_requests)} PRs) | href=https://github.com/{repo}/pulls"
        )

        for pr in pull_requests:
            # pr line
            output.append(produce_pr_output(repo, pr))
            pr_count = pr_count + 1

    print(f"💬 {pr_count} Pull Requests")

    for line in output:
        print(line)


if __name__ == "__main__":
    REPOS_TO_CHECK.sort()
    get_all_prs(REPOS_TO_CHECK)
