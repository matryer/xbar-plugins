#!/usr/bin/env python3


# This script displays your Folding@Home score.
# https://foldingathome.org/
# https://stats.foldingathome.org/donors
#
# <xbar.title>Folding@Home score</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Yuichi Tanaka</xbar.author>
# <xbar.author.github>yuichielectric</xbar.author.github>
# <xbar.desc>Displays the score of your Folding@Home account</xbar.desc>
# <xbar.image>https://raw.githubusercontent.com/yuichielectric/fah-score-bitbar-plugin/master/screen-capture.png</xbar.image>
# <xbar.dependencies>python3</xbar.dependencies>
# <xbar.abouturl>https://github.com/yuichielectric/fah-score-bitbar-plugin</xbar.abouturl>

import json
import urllib.request

# Specify your accoount name here
user_account = 'yuichielectric'


def separator(num):
    return "{:,}".format(num)


url = 'https://stats.foldingathome.org/api/donor/' + user_account
req = urllib.request.Request(url)
with urllib.request.urlopen(req) as res:
    body = json.load(res)
    print("F@h " + separator(body["credit"]))
    print('---')

    # User rank
    top = body["rank"] / body["total_users"] * 100
    print("Rank: " + separator(body["rank"]) +
          " / " + separator(body["total_users"]) +
          (" (top %.02f %%)" % top) +
          ("| href='https://stats.foldingathome.org/donor/%s'" % user_account))

    # Team
    print("---")
    for t in sorted(body["teams"], key=lambda t: t["credit"], reverse=True):
        print(t["name"] + " " + separator(t["credit"]) +
              ("| href='https://stats.foldingathome.org/team/%d'" % t["team"]))
