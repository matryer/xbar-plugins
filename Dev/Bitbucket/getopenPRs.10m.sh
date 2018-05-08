#!/bin/bash
# shellcheck disable=SC2034
# shellcheck disable=SC2154
# shellcheck source=/dev/null

# <bitbar.title>Bitbucket Pull Requests</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Mikey Beck</bitbar.author>
# <bitbar.author.github>mikeybeck</bitbar.author.github>
# <bitbar.desc>Shows Bitbucket open pull request information</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/mikeybeck/bitbar-bitbucketPRs/master/screenshot.png</bitbar.image>
# <bitbar.dependencies>jq</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/mikeybeck/bitbar-bitbucketPRs</bitbar.abouturl>

# Relevant documentation for BitBucket: http://web.archive.org/web/20150530151816/https://confluence.atlassian.com/display/BITBUCKET/pullrequests+Resource#pullrequests

USERNAME=
PASSWORD=

REPO_OWNER=
REPO_SLUG=

NUM_APPROVALS_REQ=2  # Number of approvals required for pull request

# Export PATH
export PATH="/usr/local/bin:/usr/bin:$PATH"

# Base64 icon to use in system bar
icon="iVBORw0KGgoAAAANSUhEUgAAABQAAAAWCAYAAADAQbwGAAAAAXNSR0IArs4c6QAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAACXBIWXMAAAsTAAALEwEAmpwYAAABWWlUWHRYTUw6Y29tLmFkb2JlLnhtcAAAAAAAPHg6eG1wbWV0YSB4bWxuczp4PSJhZG9iZTpuczptZXRhLyIgeDp4bXB0az0iWE1QIENvcmUgNS40LjAiPgogICA8cmRmOlJERiB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiPgogICAgICA8cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0iIgogICAgICAgICAgICB4bWxuczp0aWZmPSJodHRwOi8vbnMuYWRvYmUuY29tL3RpZmYvMS4wLyI+CiAgICAgICAgIDx0aWZmOk9yaWVudGF0aW9uPjE8L3RpZmY6T3JpZW50YXRpb24+CiAgICAgIDwvcmRmOkRlc2NyaXB0aW9uPgogICA8L3JkZjpSREY+CjwveDp4bXBtZXRhPgpMwidZAAACK0lEQVQ4EbWUO2tUQRSAb2K0CMgGsVCUoCCBlCIqNlY+QImPxsLaH5AiIhYWQsDSQgSxsRHUwsJGLYRNFbXRgGDtgqKCqEl8Jz6+b3bO3SFeDRYe+PY85txzZs7euVXVkxU9M1nh9+f4AHprtvuyXlYNk7Ef1uRMi4Rcw7iUHePSWDh2cIaEj9CBOTgOIRcxfsK5CBQ6TlKEqmo3ng8czNEJ9CdYBxfAtR/wAqZhCk7DKlB+KzpJsJ2Wej8zmA/BYgvwPdv6wX3s1aDESZMzxu9nGE1eVR1Bf4AdcAui6Dy2I/kC7tj4DVDqgjHYKwSd3R34Bh5J8Ti3wYfPg7IdnoExc2Mj6ehRucXCu5z0GK2Us3mEfzlFuz/+4xaUvd1QNVC+FoMEPfYQODOLOTdzFmEPOIImsWiS2J2OwfDVzkixmMXfwl1YCX+Ucod1F7JL2xm702gWjRqLRlLj4pKghf5azPx/KbikfrP7Xws6+Jidc1uuWby/brV+vcqHZl3JElct/FLHM971kHg2NYmEk6y6Qz8A6kOg1N0Lewv2K3gD5sb1w+zKLpSvxuHsn0LbdW32bVoe0VtzM8dG0N6wcahlEmuq9rpGB3U0x3xf4yQbsN/DxrymOgttjUjy7u6ETaDsg/XwVAfxWKK8BD/EJ3SyHEPPhBP6OoafLDt5fLsq0VQ75ulovsIDeA5PoAVpLn3o6H4AexvcAz+eTWIDb8xmcGev4Sp45/t/AVL9dQ7qDO64AAAAAElFTkSuQmCC"
response=$(curl -s -X GET --user $USERNAME:$PASSWORD "https://bitbucket.org/api/2.0/repositories/$REPO_OWNER/$REPO_SLUG/pullrequests/")
json=$(echo "$response" | jq -r -c '[.values[] | {title: .title, author: .author.display_name, num_comments: .comment_count, link_html: .links.html.href, link_status: .links.statuses.href, link_self: .links.self.href}]')
prs=$(echo "$response" | jq -r -c '(.size|tostring)')

num_approved_by_me=0
declare -a lines

for pr in $(echo "${json}" | jq -r '.[] | @base64'); do
    _jq() {
     echo "${pr}" | base64 --decode | jq -r "${1}"
    }

   build_state=$(curl -s -X GET --user $USERNAME:$PASSWORD "$(_jq '.link_status')" | jq -r '.values[].state')
   self=$(curl -s -X GET --user $USERNAME:$PASSWORD "$(_jq '.link_self')")
   num_approvals=$(echo "$self" | jq -r '[select(.participants[].approved)] | length')
   colour="red"
   if [[ $build_state == "SUCCESSFUL" ]]; then
    colour="green" # Colour to show if PR is good to go (approved & build passed)
    if [ "$num_approvals" -lt "$NUM_APPROVALS_REQ" ]; then
      colour="black" # Colour to show if PR build passed but not approved
    fi
   fi
  
   approved_by_me=$(echo "$self" | jq -r --arg USERNAME "$USERNAME" '.participants[] | select(.user.username == $USERNAME) | .approved')
   if [[ $approved_by_me == "true" ]]; then
    approved_by_me=":heavy_check_mark:"
    ((num_approved_by_me++))
   else
    approved_by_me="-"
   fi

  line="$approved_by_me $(_jq '.author') - $(_jq '.title') ┃ :heavy_check_mark: $num_approvals ┃ :speech_balloon: $(_jq '.num_comments') | href=$(_jq '.link_html') color=$colour"
  lines+=("$line")

done

# Print everything out

num_unapproved_by_me=$((prs - num_approved_by_me))
echo "$prs / $num_unapproved_by_me | templateImage=$icon dropdown=false" # Display number of PRs in menu bar
echo "---"
echo "View all open pull requests | href=https://bitbucket.org/$REPO_OWNER/$REPO_SLUG/pull-requests/"
echo "---"

for line in "${lines[@]}"
do
  echo "$line" # Display open PRs in dropdown
done
