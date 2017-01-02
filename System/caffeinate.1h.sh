#!/bin/bash

# <bitbar.title>Caffeinate</bitbar.title>
# <bitbar.version>v0.2</bitbar.version>
# <bitbar.author>Steffen Froehlich</bitbar.author>
# <bitbar.author.github>citoki</bitbar.author.github>
# <bitbar.desc>This plugin will give some caffeine, with lots of sugar, to your Mac to stay awake.
# Technically the commandline tool 'caffeinate' is executed.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/vsCWLwX.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>


function get_program_state {
  instances=$(pgrep caffeinate)

  # set state
  if [[ $instances = 1 ]]; then
    state="stopped";
  else
    state="running";
  fi
}

function set_program_icon {
  get_program_state;

  if [[ "$state" = "stopped" ]]; then
    icon="☕️";
  else
    icon="🍵";
  fi
}

# stop all previous processes
function terminate_caffeinate_instances {
  /usr/bin/killall caffeinate &> /dev/null
}

if [[ "$1" = "caffeine" ]]; then
  terminate_caffeinate_instances;
  # start caffeinate program and prevent the system and display
  # from idle sleeping
  /usr/bin/caffeinate -di;

  exit
fi

if [[ "$1" = "stop" ]]; then
  terminate_caffeinate_instances;

  exit
fi

# set program icon to default: 'not active state'
set_program_icon;

echo "$icon"
echo "---"
echo "program state: ${state}"
echo "---"
echo "Prevent machine from idle sleep | bash='$0' param1=caffeine terminal=false"
echo "Stop program | bash='$0' param1=stop terminal=false"
