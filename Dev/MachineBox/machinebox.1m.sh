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
# <bitbar.image>https://machinebox.io/assets/static/img/bitbar-plugin.png?source=bitbar</bitbar.image>
# <bitbar.dependencies>bash,jq,docker</bitbar.dependencies>
# <bitbar.abouturl>https://machinebox.io/?source=bitbar</bitbar.abouturl>

export PATH="$PATH:/usr/local/bin"
command -v jq >/dev/null 2>&1 || { echo >&2 "Click to install jq command... | href=https://stedolan.github.io/jq/?source=bitbar"; exit 0; }
command -v docker >/dev/null 2>&1 || { echo >&2 "Click to install Docker... | href=https://machinebox.io/out/docker/install?source=bitbar"; exit 0; }

if [ "$#" -gt 1 ]; then
    if [ $1 = "start" ]; then
        if [ "$MB_KEY" = "" ]; then
            osascript -e 'display notification "You need to configure your MB_KEY environment variable" with title "Machine Box" subtitle "Failed to start box"' 
            exit 0
        fi
        osascript -e 'display notification "Go to http://localhost:8080/" with title "Machine Box" subtitle "Downloading and starting..."' 
        docker run -d -p 8080:8080 -e "MB_KEY=$MB_KEY" machinebox/$2
        open http://localhost:8080
        exit 0
    fi
    if [ $1 = "stop" ]; then
        echo "stopping $2..."
        osascript -e 'display notification "Stopping..." with title "Machine Box"' 
        docker stop `docker ps -q --filter ancestor=machinebox/$2`
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
RUNNING_BOXES=`docker ps --format '{{.Image}}' | grep machinebox`

if [[ $RUNNING_BOXES = "" ]]; then 
    #echo "machinebox"
    running=""
else
    #echo $RUNNING_BOXES
    running=yes
fi

echo "---"

boxes=`curl --silent 'https://machinebox.io/api/boxes?source=bitbar' | jq -r '.boxes[].name'`

for box in $boxes; do
    if [ ! "$(docker ps | grep machinebox/$box)" ]; then
        if [[ $running = "yes" ]]; then
            echo "Start $box"
        else
            echo "Start $box | bash=$0 refresh=true terminal=false param1=start param2=$box"
        fi
    else
        echo "$box"
        echo "--Open console... | href=http://localhost:8080"
        echo "--Stop $box | bash=$0 refresh=true terminal=false param1=stop param2=$box"
    fi
done

echo "---"
echo "Open machinebox.io... | href=https://machinebox.io/account?source=bitbar"
