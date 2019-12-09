#!/bin/bash

# ----------------------------------------------------------------
# TODO: insert your MB_KEY here.
# Get your key from https://machinebox.io/account
# For more help, see https://machinebox.io/docs/setup/box-key
MB_KEY=""
# ----------------------------------------------------------------

# <bitbar.title>Machine Box</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Machina</bitbar.author>
# <bitbar.author.github>machinabot</bitbar.author.github>
# <bitbar.desc>Easily start and stop Machine Box boxes.</bitbar.desc>
# <bitbar.image>http://i.imgur.com/gkOYSWH.png</bitbar.image>
# <bitbar.dependencies>bash,jq,docker</bitbar.dependencies>
# <bitbar.abouturl>https://machinebox.io/?source=bitbar</bitbar.abouturl>

export PATH="$PATH:/usr/local/bin"
command -v jq >/dev/null 2>&1 || { echo >&2 "Click to install jq command... | href=https://stedolan.github.io/jq/?source=bitbar"; exit 0; }
command -v docker >/dev/null 2>&1 || { echo >&2 "Click to install Docker... | href=https://machinebox.io/out/docker/install?source=bitbar"; exit 0; }

if [ "$#" -gt 1 ]; then
    if [ "$1" = "start" ]; then
        if [ "$MB_KEY" = "" ]; then
            osascript -e 'display notification "You need to configure your MB_KEY environment variable" with title "Machine Box" subtitle "Failed to start box"' 
            exit 0
        fi
        osascript -e 'display notification "Go to http://localhost:8080/" with title "Machine Box" subtitle "Downloading and starting..."' 
        docker run -d -p 8080:8080 -e "MB_KEY=$MB_KEY" "machinebox/$2"
        open http://localhost:8080
        exit 0
    fi
    if [ "$1" = "stop" ]; then
        echo "stopping $2..."
        osascript -e 'display notification "Stopping..." with title "Machine Box"' 
        docker stop "$(docker ps -q --filter ancestor="machinebox/$2")"
        osascript -e 'display notification "Box has been stopped" with title "Machine Box" subtitle "Stopped"' 
        exit 0
    fi
fi

echo " | image=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"

if ! docker ps >/dev/null 2>&1; then 
    echo "---"
    echo "Docker daemon isn't running"
    echo "---"
    echo "Learn more about running Docker... | href=https://machinebox.io/docs/setup/docker?source=bitbar"
    exit 0
fi

if [ "$MB_KEY" = "" ]; then
    echo "---"
    echo "You need to Setup MB_KEY"
    echo "by editing the machinebox.sh plugin file"
    echo "---"
    echo "Get a free MB_KEY... | href=https://machinebox.io/account?source=bitbar"
    exit 1
fi

# list running boxes at the top
RUNNING_BOXES=$(docker ps --format '{{.Image}}' | grep machinebox)

running=""
if [[ "$RUNNING_BOXES" = "" ]]; then 
    running=""
else
    running="yes"
fi

echo "---"

boxes=$(curl --silent 'https://machinebox.io/api/boxes?source=bitbar' | jq -r '.boxes[].name')

for box in $boxes; do
    if [ "$(docker ps | grep -c "machinebox/$box")" = 1 ]; then
        echo "$box"
        echo "--Open console... | href=http://localhost:8080"
        echo "--Stop $box | bash=$0 refresh=true terminal=false param1=stop param2=$box"
    else
        if [[ "$running" = "yes" ]]; then
            echo "Start $box"
        else
            echo "Start $box | bash=$0 refresh=true terminal=false param1=start param2=$box"
        fi
    fi
done

echo "---"
echo "Open machinebox.io... | href=https://machinebox.io/account?source=bitbar"
