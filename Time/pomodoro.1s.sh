#!/bin/bash
#
# <bitbar.title>Pomodoro Timer</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Goran Gajic</bitbar.author>
# <bitbar.author.github>gorangajic</bitbar.author.github>
# <bitbar.desc>Pomodoro Timer that uses Pomodoro Technique™</bitbar.desc>
# <bitbar.image>http://i.imgur.com/T0zFY89.png</bitbar.image>

WORK_TIME=25
BREAK_TIME=3

SAVE_LOCATION=$TMPDIR/bitbar-promodo
TOMATO='🍅'

WORK_TIME_IN_SECONDS=$((WORK_TIME * 60))
BREAK_TIME_IN_SECONDS=$((BREAK_TIME * 60))

CURRENT_TIME=$(date +%s)

if [ -f "$SAVE_LOCATION" ];
then
    DATA=$(cat "$SAVE_LOCATION")

else
    DATA="$CURRENT_TIME|0"
fi

TIME=$(echo "$DATA" | cut -d "|" -f1)
STATUS=$(echo "$DATA" | cut -d "|" -f2)

function changeStatus {
    echo "$CURRENT_TIME|$1" > "$SAVE_LOCATION";
    osascript -e "display notification \"$2\" with title \"$TOMATO Pomodoro\" sound name \"$3\"" &> /dev/null
}

function breakMode {
    changeStatus "2" "Break Mode" "Glass"
}

function workMode {
    changeStatus "1" "Work Mode" "Blow"
}

case "$1" in
"work")
    workMode
    exit
  ;;
"break")
    breakMode
    exit
  ;;
"disable")
    changeStatus "0" "Disabled"
    exit
  ;;
esac



function timeLeft {
    local FROM=$1
    local TIME_DIFF=$((CURRENT_TIME - TIME))
    local TIME_LEFT=$((FROM - TIME_DIFF))
    echo "$TIME_LEFT";
}

function getSeconds {
    echo $(($1 % 60))
}

function getMinutes {
    echo $(($1 / 60))
}

function printTime {
    SECONDS=$(getSeconds "$1")
    MINUTES=$(getMinutes "$1")
    printf "%s %02d:%02d| color=%s\n" "$TOMATO" "$MINUTES" "$SECONDS"  "$2"
}

case "$STATUS" in
# STOP MODE
"0")
    echo "$TOMATO"
  ;;
"1")
    TIME_LEFT=$(timeLeft $WORK_TIME_IN_SECONDS)
    if (( "$TIME_LEFT" < 0 )); then
        breakMode
    fi
    printTime "$TIME_LEFT" "red"
  ;;
"2")
    TIME_LEFT=$(timeLeft $BREAK_TIME_IN_SECONDS)
    if (("$TIME_LEFT" < 0)); then
        workMode
    fi
    printTime "$TIME_LEFT" "green"
  ;;
esac

echo "---";
echo "👔 Work | bash=\"$0\" param1=work terminal=false"
echo "☕ Break | bash=\"$0\" param1=break terminal=false"
echo "🔌 Disable | bash=\"$0\" param1=disable terminal=false"
