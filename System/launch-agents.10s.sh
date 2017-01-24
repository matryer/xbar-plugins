#!/usr/bin/env bash

# <bitbar.title>Launch Agents</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Paul W. Rankin</bitbar.author>
# <bitbar.author.github>rnkn</bitbar.author.github>
# <bitbar.desc>Shows and manages user Launch Agents.</bitbar.desc>
# <bitbar.image>http://photos.paulwrankin.com/screenshots/launch-agents.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>

# BitBar Launch Agents plugin

launchctl=$(which launchctl)
defaults=$(which defaults)
open=$(which open)
services=("$HOME/Library/LaunchAgents/"*.plist)

if [[ $1 = start ]]
then "$launchctl" start "$2"
fi

if [[ $1 = stop ]]
then "$launchctl" stop "$2"
fi

if [[ $1 = load ]]
then "$launchctl" load "$2"
fi

if [[ $1 = unload ]]
then "$launchctl" unload "$2"
fi

if [[ $1 = reload ]]
then "$launchctl" unload "$2"
     "$launchctl" load "$2"
fi

# if [[ $1 = viewlog ]]
# # shellcheck disable=SC2046
# then open -a Console $("$defaults" read "$2" StandardOutPath)
# fi

# if [[ $1 = viewerror ]]
# # shellcheck disable=SC2046
# then open -a Console $("$defaults" read "$2" StandardErrorPath)
# fi

function service_pid {
    "$launchctl" list | grep "$1" | sed -E 's/^([-0-9]+).*([0-9]+).*/\1/'
}

function service_status {
    "$launchctl" list | grep "$1" | sed -E 's/^([-0-9]+).*([0-9]+).*/\2/'
}

function service_property {
    "$defaults" read "$1" "$2" 2> /dev/null
}

echo "🚀"
echo "---"
for service in "${services[@]}"
do label=$(service_property "$service" Label)
   log=$(service_property "$service" StandardOutPath)
   errorlog=$(service_property "$service" StandardErrorPath)
   pid=$(service_pid "$label")
   status=$(service_status "$label")
   if [[ $pid = "-" ]]
   then pid=-1
   fi
   if [[ $pid -eq -1 && $status -eq 0 ]]
   then echo "$label | color=#00a4db"
        echo "--Unload | bash='$0' param1=unload param2=$service terminal=false refresh=true"
        echo "--Reload | bash='$0' param1=reload param2=$service terminal=false refresh=true"
        echo "--Start | bash='$0' param1=start param2=$label terminal=false refresh=true"
        echo "-----"
        if [[ -f $log ]]
        then echo "--Status: Idle | bash=$open param1=-a param2=Console param3=$log terminal=false"
        else echo "--Status: Idle"
        fi
        if [[ -f $errorlog ]]
        then echo "--Errors: None | bash=$open param1=-a param2=Console param3=$errorlog terminal=false"
        else echo "--Errors: None"
        fi
   elif [[ $pid -gt 0 && $status -eq 0 ]]
   then echo "$label | color=green"
        echo "--Unload | bash='$0' param1=unload param2=$service terminal=false refresh=true"
        echo "--Reload | bash='$0' param1=reload param2=$service terminal=false refresh=true"
        echo "--Stop | bash='$0' param1=stop param2=$label terminal=false refresh=true"
        echo "-----"
        if [[ -f $log ]]
        then echo "--Status: Running | bash=$open param1=-a param2=Console param3=$log terminal=false"
        else echo "--Status: Running"
        fi
        if [[ -f $errorlog ]]
        then echo "--Errors: None | bash=$open param1=-a param2=Console param3=$errorlog terminal=false"
        else echo "--Errors: None"
        fi
   elif [[ $status -gt 0 ]]
   then echo "$label | color=red"
        echo "--Unload | bash='$0' param1=unload param2=$service terminal=false refresh=true"
        echo "--Reload | bash='$0' param1=reload param2=$service terminal=false refresh=true"
        echo "-----"
        if [[ -f $log ]]
        then echo "--Status: Stopped | bash=$open param1=-a param2=Console param3=$log terminal=false"
        else echo "--Status: Stopped"
        fi
        if [[ -f $errorlog ]]
        then echo "--Errors: $status | bash=$open param1=-a param2=Console param3=$errorlog terminal=false"
        else echo "--Errors: $status"
        fi
   else echo "$label"
        echo "--Load | bash='$0' param1=load param2=$service terminal=false refresh=true"
        echo "-----"
        if [[ -f $log ]]
        then echo "--Status: Stopped | bash=$open param1=-a param2=Console param3=$log terminal=false"
        else echo "--Status: Stopped"
        fi
   fi
done
echo "---"
echo "Refresh | refresh=true"
