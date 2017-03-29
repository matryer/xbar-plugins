#!/bin/bash

# <bitbar.title>Daily Standup</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Krishna Kumar</bitbar.author>
# <bitbar.author.github>krishkumar</bitbar.author.github>
# <bitbar.desc>Displays a list of all your git commits in the last 24 hours. Indicates days you have been chilling without committing any code. </bitbar.desc>
# <bitbar.image>http://raw.githubusercontent.com/krishkumar/daily-standup/master/daily-standup-gold.png</bitbar.image>
# <bitbar.abouturl>https://github.com/krishkumar/daily-standup</bitbar.abouturl>
#
# Dependencies: You need to have git-standup installed.
# To install git-standup on OSX with homebrew, open Terminal and run - $ brew install git-standup
# For other installation options, see this page - https://github.com/kamranahmedse/git-standup


# Constants
COMMITINDICATOR=""
GOLDMEDAL=🥇 # More than 5 commits today
SILVERMEDAL=🥈 # More than 3 commits today
BRONZEMEDAL=🥉 # Atleast 1 commit today
PROJECTINDICATOR=📂
EMPTY_INDICATOR=❄️
ALERT_INDICATOR=🚨
DAYS_SINCE_COMMIT_MESSAGE="Earn medals... Get to work! | color=blue"
DAYS_SINCE_COMMIT_CONTENT=" days since last commit "
user=$(whoami)
PROJECTS_DIRECTORY="/Users/$user/projects/"
DAYS_SINCE_COMMIT=0

# Start
COUNT=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup | grep -c "<$user>")
COUNT="$COUNT"
if [ "$COUNT" = 0 ]
then
    COUNT=""
    COMMITINDICATOR=$EMPTY_INDICATOR
    DAYS_SINCE_COMMIT=1
    COUNT2=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 2 | grep -c "<$user>")
    COUNT3=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 3 | grep -c "<$user>") 
    COUNT4=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 4 | grep -c "<$user>") 
    COUNT5=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 5 | grep -c "<$user>") 
    COUNT6=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 6 | grep -c "<$user>") 
    COUNT7=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 7 | grep -c "<$user>") 
    COUNT8=$(cd "$PROJECTS_DIRECTORY" || exit ;/usr/local/bin/git-standup -d 8 | grep -c "<$user>") 
    if [ "$COUNT2" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=2
    fi
    if [ "$COUNT3" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=3
    fi
    if [ "$COUNT4" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=4
    fi
    if [ "$COUNT5" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=5
    fi
    if [ "$COUNT6" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=6
    fi
    if [ "$COUNT7" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR$EMPTY_INDICATOR
      DAYS_SINCE_COMMIT=7
    fi
    if [ "$COUNT8" = 0 ]
    then
      COUNT=""
      COMMITINDICATOR=$ALERT_INDICATOR
      DAYS_SINCE_COMMIT=7+
    fi
fi
if [[ $COUNT -gt 0 ]]
  then
  #statements
  COMMITINDICATOR=$BRONZEMEDAL
fi
if [[ $COUNT -gt 2 ]]
 then
  #statements
  COMMITINDICATOR=$SILVERMEDAL
fi
if [[ $COUNT -gt 5 ]]
  then
  #statements
  COMMITINDICATOR=$GOLDMEDAL
fi
echo $COMMITINDICATOR

echo ---

# On Click
cd "$PROJECTS_DIRECTORY" || exit 
STANDUP=$(/usr/local/bin/git-standup)
if [[ -z "$STANDUP" ]]; then
  echo "$DAYS_SINCE_COMMIT| size=30 "
  echo "$DAYS_SINCE_COMMIT_CONTENT"
  echo "$DAYS_SINCE_COMMIT_MESSAGE"
elif [[ -n "$STANDUP" ]]; then
  /usr/local/bin/git-standup | sed "s@$PROJECTS_DIRECTORY@$PROJECTINDICATOR@;n"
fi
