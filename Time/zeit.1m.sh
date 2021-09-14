#!/bin/sh

# <xbar.title>zeit</xbar.title>
# <xbar.author>Marius</xbar.author>
# <xbar.author.github>mrusme</xbar.author.github>
# <xbar.desc>Control `zeit` (https://github.com/mrusme/zeit) from the macOS menu bar.</xbar.desc>
# <xbar.image>https://github.com/mrusme/zeit/raw/main/documentation/zeit.png</xbar.image>
# <xbar.abouturl>https://マリウス.com/zeit-erfassen-a-cli-activity-time-tracker/</xbar.abouturl>
# <xbar.version>1.0</xbar.version>
#
# <xbar.var>string(ZEIT_BIN="/usr/local/bin/zeit"): Your zeit binary location</xbar.var>
# <xbar.var>string(ZEIT_DB="$HOME/.zeit.db"): Your zeit database location</xbar.var>
#
# Control `zeit` (https://github.com/mrusme/zeit) from the macOS menu bar.
#
# by Marius (marius@xn--gckvb8fzb.com)
#

PLACEHOLDER_NO_PROJECT='[no project]'
PLACEHOLDER_NO_TASK='[no task]'

if [ -z "$ZEIT_BIN" ]
then
  ZEIT_BIN=$1
fi

if [ -z "$ZEIT_DB" ]
then
  export ZEIT_DB=$2
fi

case $3 in
  "track")
    flag_p=$4
    flag_t=$5

    $ZEIT_BIN --no-colors finish

    if [ "$flag_p" = "$PLACEHOLDER_NO_PROJECT" ]
    then
      flag_p=''
    fi
    if [ "$flag_t" = "$PLACEHOLDER_NO_TASK" ]
    then
      flag_t=''
    fi

    $ZEIT_BIN --no-colors track -p "$flag_p" -t "$flag_t"
    # exit 0
    ;;
  "finish")
    $ZEIT_BIN --no-colors finish
    # exit 0
    ;;
esac

trackingProject=''
trackingTask=''
trackingDuration=''
tracking=$($ZEIT_BIN --no-colors tracking)

if echo "$tracking" | grep -q '^ ▶ tracking'
then
  if echo "$tracking" | grep -q '^ ▶ tracking task for'
  then
    trackingProject=$PLACEHOLDER_NO_PROJECT
    trackingTask=$PLACEHOLDER_NO_TASK
    trackingDuration=$(echo "$tracking" | sed -E 's/.*tracking task for (.+)/\1/g')
  else
    trackingProject=$(echo "$tracking" | sed -E 's/.*tracking (.+) on (.+) for (.+)/\2/g')
    trackingTask=$(echo "$tracking" | sed -E 's/.*tracking (.+) on (.+) for (.+)/\1/g')
    trackingDuration=$(echo "$tracking" | sed -E 's/.*tracking (.+) on (.+) for (.+)/\3/g')
  fi
  tracking=$trackingDuration
fi

echo "$tracking"
echo '---'
echo 'Projects'

project=''
$ZEIT_BIN --no-colors list --only-projects-and-tasks | while read -r line
do
  if echo "$line" | grep -q '^◆'
  then
    project=$(echo "$line" | sed 's/◆[[:space:]]\{0,3\}//g')
    if [ "$project" = "" ]
    then
      project=$PLACEHOLDER_NO_PROJECT
    fi

    if [ "$project" = "$trackingProject" ]
    then
      echo "-- ▶ $project"
    else
      echo "-- $project"
    fi
  elif echo "$line" | grep -q '^└──'
  then
    task=$(echo "$line" | sed 's/└──[[:space:]]\{0,3\}//g')

    if [ "$task" = "" ]
    then
      task=$PLACEHOLDER_NO_TASK
    fi

    if [ "$project" = "$trackingProject" ] && [ "$task" = "$trackingTask" ]
    then
      echo "---- ▶ $task | shell='$0' param1='$ZEIT_BIN' param2='$ZEIT_DB' param3=finish param4='$project' param5='$task' terminal=false refresh=true"
    else
      echo "---- $task | shell='$0' param1='$ZEIT_BIN' param2='$ZEIT_DB' param3=track param4='$project' param5='$task' terminal=false refresh=true"
    fi
  fi
done
exit
