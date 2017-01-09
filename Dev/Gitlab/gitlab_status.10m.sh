#!/usr/bin/env bash
# <bitbar.title>Gitlab status plugin</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Brett Jones</bitbar.author>
# <bitbar.author.github>blockloop</bitbar.author.github>
# <bitbar.image>https://dl.dropbox.com/s/r4st2fnfhy7tcsv/gitlab-status-screenshot-bitbar.png</bitbar.image>
# <bitbar.desc>Shows the current status of status.gitlab.com. Find out if Gitlab is having DDOS problems which will affect pushes/pulls.</bitbar.desc>
# <bitbar.dependencies>python</bitbar.dependencies>
#

rawfeed="$(curl -SsL https://status.gitlab.com/ | grep OK)"
feed="$(echo "$rawfeed" | sed 's/<h5><span class="label label-.*">//g' | sed 's/<\/span>//g' | sed 's/<\/h5>//g')"

if [[ $rawfeed == *"label-error"* ]]; then
    echo "GL: 𝙭 | color=red"
elif [[ $rawfeed == *"label-warning"* ]]; then
    echo "GL: 𝙭 | color=orange"
else
    echo "GL: ✓"
fi

echo "---"
while read -r l; do
    echo "$l"
done <<< "$feed"
echo "status.gitlab.com | href=https://status.gitlab.com/"
