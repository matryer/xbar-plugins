#!/bin/bash
#
# <bitbar.title>GitHub pull-requess plugin</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Jasmin Begic</bitbar.author>
# <bitbar.author.github>jasminb</bitbar.author.github>
# <bitbar.desc>Plugin shows currently open pull-requests on configured github repository. Items shown in the list are clickable (clicking the link opens up GitHub PR page).</bitbar.desc>
# <bitbar.dependencies>node,curl</bitbar.dependencies>

export PATH='/usr/local/bin:/usr/bin:$PATH'

# API base path
GITHUB_REPO_API="https://api.github.com/repos"

# User with permission to the repository
GITHUB_USER=""

# GitHub API access token (can be generated via github website)
GITHUB_ACCESS_TOKEN=""

# Name of the repository to fetch pull requests for
GITHUB_REPO=""

# Owner of the repository
GITHUB_REPO_OWNER=""

# JavaScript used to parse result JSON and extract titile/url/submitter of pull request
JS='if(prJson=JSON.parse(process.argv[1]),prJson.length>0)for(console.log("♣︎ " +prJson.length+" PRs "),console.log("---"),i=0;i<prJson.length;i++)console.log("▸ "+prJson[i].title+" ("+prJson[i].user.login+") | href="+prJson[i].html_url);"";'

# Fetch and parse open pull request from GitHub
node -pe "$JS" "$(curl -s -u $GITHUB_USER:$GITHUB_ACCESS_TOKEN $GITHUB_REPO_API/$GITHUB_REPO_OWNER/$GITHUB_REPO/pulls)"
