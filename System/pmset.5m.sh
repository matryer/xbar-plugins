#!/usr/bin/env bash

if [[ "$1" = "stop_caffeinate" ]]; then
  pkill caffeinate
  exit
fi

if [[ "$1" = "start_caffeinate" ]]; then
  /usr/bin/caffeinate -sdi &
  exit
fi

if [[ "$1" = "edit_this_script" ]]; then
  open -b com.sublimetext.3 "$0"
  exit
fi

sleep_disabled=$(pmset -g | grep SleepDisabled | awk '{print $2}')

is_caffeinate_running=$(pgrep caffeinate)

if [[ $sleep_disabled == "0" ]]; then

  if [[ $is_caffeinate_running -eq 0 ]] ; then  # "1" = no processes found
    echo "🔵"
    status="💤 Sleeping normally"
  else
    echo "☕️"
    status="caffeinating... ☕️"
  fi
  cmd="🔋 pmset: Disable sleep on battery | bash=sudo param1=/usr/bin/pmset param2=-a param3=disablesleep param4=1 terminal=true refresh=true"
else
  echo "‼️"
  status="‼️ Preventing sleep on battery"
  cmd="🔥 pmset: RE-enable sleep on battery | bash=sudo color=indianred param1=/usr/bin/pmset param2=-a param3=disablesleep param4=0 terminal=true refresh=true"
fi

echo '---'
echo "$status"
echo '---'
echo "$cmd"

if [[ $is_caffeinate_running -eq 0 ]] ; then
  echo "☕️ caffeinate | bash='$0' param1=start_caffeinate refresh=true terminal=false"
else
  echo "❌ [caffeinate already running] KILL NOW | bash='$0' color=indianred param1=stop_caffeinate refresh=true terminal=false"
fi
echo '---'
echo "Edit this file | bash='$0' param1="edit_this_script" terminal=false"

echo '---'
echo "🔃 Refresh... | refresh=true"
