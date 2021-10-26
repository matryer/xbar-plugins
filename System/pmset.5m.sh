#!/usr/bin/env bash

if [[ "$1" = "pmset_disable_battery_sleep" ]]; then
  osascript -e 'display dialog "Disable system sleep while on battery?" buttons {"Cancel", "DISABLE NOW! (System will NOT sleep!)"}
do shell script "/usr/bin/pmset -a disablesleep 1" with administrator privileges'
  exit
fi

if [[ "$1" = "pmset_restore_battery_sleep" ]]; then
  osascript -e 'display dialog "Restore sleep mode back to normal?" buttons {"Cancel", "RESTORE NORMAL SLEEP"}
do shell script "/usr/bin/pmset -a disablesleep 0" with administrator privileges'
  exit
fi

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
  cmd="🔋 pmset: Disable sleep on battery | bash='$0' param1=pmset_disable_battery_sleep terminal=false refresh=true"
else
  echo "‼️"
  status="‼️ Preventing sleep on battery"
  cmd="🔥 pmset: RESTORE sleep on battery | bash='$0' color=indianred param1=pmset_restore_battery_sleep terminal=false refresh=true"
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
echo "✏️ Edit this file | bash='$0' param1="edit_this_script" terminal=false"
echo '---'
echo "🔃 Refresh... | refresh=true"
