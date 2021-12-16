#!/usr/bin/env bash

# <xbar.title>What Keeps Me Awake?</xbar.title>
# <xbar.version>1.0</xbar.version>
# <xbar.author>Sebastian Tauchert</xbar.author>
# <xbar.author.github>s1e2b3i4</xbar.author.github>
# <xbar.desc>Show apps that may be preventing the Mac from going to sleep.</xbar.desc>
# <xbar.image>https://i.imgur.com/haJr7L7.png</xbar.image>
# <xbar.dependencies>bash</xbar.dependencies>

if [ "$XBARDarkMode" == "true" ]; then
    TEXT_COLOR="white"
else
    TEXT_COLOR="black"
fi

function get_apps () {
    apps=($(pmset -g assertions |grep ".*pid.*PreventUserIdleSystemSleep" | cut -d "(" -f2 | cut -d ")" -f1 | sort | uniq))
}

function get_app_details () {
    app_name="$1"
    app_details=($(pmset -g assertions |grep ".*pid.*(${app_name}).*PreventUserIdleSystemSleep" | cut -d ":" -f5- | awk '{$1=$1};1' | tr \  _))
}

function print_detail () {
    detail=$1
    stripped_detail=$(echo "$detail" | cut -d "\"" -f2)
    config_str="color=#909090"
    echo "--${stripped_detail} | ${config_str}"
}

function print_kill () {
    app_name=$1
    pid=$(pmset -g assertions |grep ".*pid.*(${app_name}).*PreventUserIdleSystemSleep" | cut -d " " -f5 | cut -d "(" -f1 | uniq)
    config_str="color=red | shell=kill | param1=${pid} | refresh=true"
    echo "-----"
    echo "--Kill Process (${pid})| ${config_str}"
}

function set_icon () {
    apps=($(pmset -g assertions |grep ".*pid.*PreventUserIdleSystemSleep"))
    if [ "${#apps[@]}" == 0 ]; then
        echo "☀ | size=18 | color=#B2B2B2"
    else
        echo "☀ | size=18 | color=${TEXT_COLOR}"
    fi
}

function print_man_desc () {
    app=$1
    desc=$(man $app | col -bx | grep -A10 "^DESCRIPTION" | tail -n+2 | sed 's/^[ \t]*//' | tr \\n \\0 | cut -d "." -f1)
    if [[ ! -z "$desc" ]]; then
        config_str="color=${TEXT_COLOR} | shell=osascript param1=-e param2='tell application \"Terminal\" to activate' param3=-e param4='tell application \"Terminal\" to do script \"man ${app}\""
        echo "--${desc}. | ${config_str}"
        echo "-----"
    fi
}

function main () {
    set_icon
    echo '---'
    get_apps
    for app in "${apps[@]}"
    do
        echo "${app}"
        print_man_desc "$app"
        get_app_details "$app"
        for detail in "${app_details[@]}"
        do
            print_detail "$detail"
        done
        print_kill "$app"
    done
}

main
