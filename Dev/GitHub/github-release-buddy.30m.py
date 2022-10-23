#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <xbar.title>GitHub Relase buddy</bitbar.title>
# <xbar.version>v1.0</bitbar.version>
# <xbar.author>Luis Almeida Santos</bitbar.author>
# <xbar.author.github></bitbar.author.github>
# <xbar.desc>This plugin displays the latest Release in GitHub repositories.</bitbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/71266511/197400687-954bfac6-0acf-499e-87ca-99dc01c3d509.png</xbar.image>
# <xbar.dependencies>python</bitbar.dependencies>

# <xbar.var>string(USERNAME=""): Your GitHub username. (ie: luis-santos-teampicnic)</xbar.var>
# <xbar.var>string(TOKEN=""): A GitHub Personal access tokens with, at least, the repo scope. (https://github.com/settings/tokens/new)</xbar.var>
# <xbar.var>string(REPOS_TO_CHECK="PicnicSupermarket/jolo,PicnicSupermarket/hiring-experience,PicnicSupermarket/oss-parent,PicnicSupermarket/nepsnowplow,PicnicSupermarket/diepvries,PicnicSupermarket/reactive-support,PicnicSupermarket/employer-statement-generator"): Comma separated list of repositories to check.</xbar.var>

# Author(s):
#  * Luis Almeida Santos (github.com/luis-santos-teampicnic)
#
# Changelog:
# 1.0
#   Initial version

import base64
import json
import os
import sys
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime

# Configure the plugin from the xbar variables declared above

USERNAME = os.environ.get("USERNAME")
TOKEN = os.environ.get("TOKEN")
REPOS_TO_CHECK = os.environ.get("REPOS_TO_CHECK", "").split(",")

# Generic configuration to connect to the GitHub API
BASE_URL = "https://api.github.com"
AUTHORIZATION = "Basic " + base64.b64encode(
    f"{USERNAME}:{TOKEN}".encode("utf-8")
).decode("utf-8")
USER_AGENT = f"{USERNAME} - releasebuddy - xbar"


def __get(uri):
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
        if e.code == 404:
            return None
        else:
            print("🛑  HTTP GET Error!")
            print("---")
            print(
                f"Check if you have the correct access code in the plugin - could also be an API/HTTP error [{BASE_URL}{uri}]"
            )
            print(e)
            sys.exit(1)


def __get_latest_release(repo):
    return __get(f"/repos/{repo}/releases/latest")


def __produce_output(repos):
    output = []
    output.append("---")
    for repo in repos:
        release = __get_latest_release(repo)
        
        if release is None:
            output_msg = "[{}] No releases yet! | href=https://github.com/{}/releases color=red".format(
                repo,
                repo
            )
        else:
            created_at = datetime.strptime(release["created_at"], "%Y-%m-%dT%H:%M:%SZ")
            days_old = (datetime.now() - created_at).days
            output_msg = "[{}] {} – {} days old | href={}".format(
                repo,
                release['name'],
                days_old,
                release['html_url']
            )
        
        output.append(output_msg)


    print(f"💬 GitHub Releases")

    for line in output:
        print(line)


if __name__ == "__main__":
    if not USERNAME or not TOKEN:
        print("GitHub Release buddy")
        print("---")
        print("Please set up the username and token in the plugin configuration!")
    else:
        REPOS_TO_CHECK.sort()
        __produce_output(list(filter(None, REPOS_TO_CHECK)))
