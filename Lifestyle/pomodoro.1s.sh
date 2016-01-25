#!/bin/bash
# <bitbar.title>Pomodoro timer</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Jan van den Berg</bitbar.author>
# <bitbar.author.github>koozz</bitbar.author.github>
# <bitbar.desc>Provides a minimal Pomodoro timer</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>


# Icon; http://emojipedia.org/tomato
POMODORO="🍅"
#POMODORO="\xF0\x9F\x8D\x85"  # echo -e $POMODORO
POMODORO_SECONDS=1500
POMODORO_DATA="$(dirname $0)/pomodoro.data"

# Ensure data file
[[ ! -f $POMODORO_DATA ]] && touch $POMODORO_DATA && echo "POMODORO_END=" > $POMODORO_DATA

# Load data file
source $POMODORO_DATA

# Pomodoro menu
pomodoro-menu () {
  if [[ -z $POMODORO_END ]]; then
    echo "$POMODORO"
    echo "---"
    echo "Start pomodoro | terminal=false bash=$0 param1=start"
  else
    local now=$(date +"%s")
    local diff=$(($POMODORO_END-$now))
    if [[ $diff < 0 ]]; then
      echo "$POMODORO 0:00 | color=red"
      echo "---"
      echo "Clear | terminal=false bash=$0 param1=stop"
    else
      printf "%s %d:%.2d\n" $POMODORO $(($diff / 60)) $(($diff % 60))
      echo "---"
      echo "Stop pomodoro | terminal=false bash=$0 param1=stop"
    fi
  fi
}

# Starting a new pomodoro (extend with cylces, break-time etc.)
pomodoro-start () {
  local now=$(date +"%s")
  local end=$(( $now + $POMODORO_SECONDS ))
  (
    echo "POMODORO_END=$end"
  ) > $POMODORO_DATA
}

# Stopping a pomodoro
pomodoro-stop () {
  (
    echo "POMODORO_END="
  ) > $POMODORO_DATA
}

# Main
case "$1" in
"start")
  pomodoro-start
  ;;
"stop" )
  pomodoro-stop
  ;;
*)
  pomodoro-menu
  ;;
esac
