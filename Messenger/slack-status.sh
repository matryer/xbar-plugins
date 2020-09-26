#!/bin/bash

# <bitbar.title>Slack Stuatus</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Ben Sehl</bitbar.author>
# <bitbar.author.github>benjaminsehl</bitbar.author.github>
# <bitbar.desc>Change your status on Slack from the menubar.</bitbar.desc>
# <bitbar.dependencies></bitbar.dependencies>

# Sign up for Statushook
BASE='https://api.statushook.cool/v1/prod/webhook/fire?id='

# To keep things tidy, put the ids for your webhooks in variables
FOCUS=''
VACATION=''
DONE=''
BREAK=''
MEETING=''
LUNCH=''

case $1 in 
  'focus')
    curl -s $BASE$FOCUS
    ;;
  'meeting')
    curl -s $BASE$MEETING
  ;;
  'break')
    curl -s $BASE$BREAK
  ;;
  'lunch')
    curl -s $BASE$LUNCH
  ;;
  'done')
    curl -s $BASE$DONE
  ;;
  'vacation')
    curl -s $BASE$VACATION
  ;;
esac


echo "| image=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"
echo "---"
echo ":hear_no_evil: Focus | bash='$0' param1=focus terminal=false refresh=true"
echo ":calendar: In a meeting | bash='$0' param1=meeting terminal=false refresh=true"
echo ":walking: Taking a break | bash='$0' param1=break terminal=false refresh=true"
echo ":hamburger: Eating lunch | bash='$0' param1=lunch terminal=false refresh=true"
echo ":sunglasses: Done for the day | bash='$0' param1=done terminal=false refresh=true"
echo ":palm_tree: On vacation  | bash='$0' param1=vacation terminal=false refresh=true"
