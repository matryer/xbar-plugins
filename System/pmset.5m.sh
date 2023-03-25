#!/usr/bin/env bash
# <bitbar.title>Sleep Manager</bitbar.title>
# <bitbar.version>v0.3</bitbar.version>
# <bitbar.author>glowinthedark</bitbar.author>
# <bitbar.author.github>glowinthedark</bitbar.author.github>
# <bitbar.desc>Sleeping and power management GUI. View current sleep status, disable sleep on battery, prevent sleeping using ootb caffeinate</bitbar.desc>
# <bitbar.image>https://telegra.ph/file/20ef9918b679e238bc5ff.png</bitbar.image>
# <bitbar.abouturl>https://github.com/matryer/xbar-plugins/blob/main/System/pmset.5m.sh</bitbar.abouturl>

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
  /usr/bin/pkill caffeinate
  exit
fi

if [[ "$1" = "start_caffeinate" ]]; then
  /usr/bin/caffeinate -sdi &
  exit
fi

if [[ "$1" = "edit_this_script" ]]; then
  # open with default editor registered for .sh extension
  open "$0";
  # OR use an explicit editor by app name or by bundle
  # open -a 'Sublime Text' "$0"
  # open -b com.sublimetext.4 "$0"
  exit
fi

battery_stats="$(pmset -g)"
sleep_disabled=$(echo "$battery_stats" | awk '$1~/SleepDisabled/ {print $2}')
sleep_prevented=$(echo "$battery_stats" | awk -F'(' '$0~/\(sleep prevented/ {print $2}' | tr -d ')')
display_sleep_prevented=$(echo "$battery_stats" | awk -F'(' '$0~/\(display sleep prevented/ {print $2}' | tr -d ')')

/usr/bin/pgrep -n caffeinate -q &>/dev/null
is_caffeinate_running="$?"

if [[ $sleep_disabled == "0" ]]; then
  if [[ $is_caffeinate_running -eq 0 ]] ; then  # "1" = no processes found
    echo "☕️"
    status="caffeinating... ☕️"
  else
    if [[ "_$sleep_prevented" != "_" ]] ; then
      echo '♨️'
      status="♨️ Sleep prevented by application(s):"
    else
      echo "🔋"
      status="💤 Sleeping normally"
    fi
  fi
  cmd="🔋 pmset: Disable sleep on battery | bash='$0' param1=pmset_disable_battery_sleep terminal=false refresh=true"
else
  echo "‼️"
  status="‼️ Preventing sleep on battery"
  cmd="🔥 pmset: RESTORE sleep on battery | bash='$0' color=indianred param1=pmset_restore_battery_sleep terminal=false refresh=true"
fi

echo '---'
echo "$status"
if [[ "_$sleep_prevented" != "_" ]] ; then
  echo '---'
  echo "❗️$sleep_prevented"
fi
if [[ "_$display_sleep_prevented" != "_" ]] ; then
  echo '---'
  echo "‼️$display_sleep_prevented"
fi
echo '---'
echo "$cmd"

if [[ $is_caffeinate_running -eq 0 ]] ; then
  echo "❌ DECAFFEINATE! | bash='$0' color=indianred param1=stop_caffeinate refresh=true terminal=false"
else
  echo "☕️ caffeinate | bash='$0' param1=start_caffeinate refresh=true terminal=false"
fi
echo '---'
echo "✏️ Edit this file | bash='$0' param1="edit_this_script" terminal=false"
echo '---'
echo "🔃 Refresh... | refresh=true"
