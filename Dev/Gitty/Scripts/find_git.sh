#!/bin/sh
# <bitbar.title>Gitty</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Timothy Barnard</bitbar.author>
# <bitbar.author.github>collegboi</bitbar.author.github>
# <bitbar.image>https://raw.githubusercontent.com/collegboi/Bitbar-Gitty/master/image1.png</bitbar.image>
# <bitbar.desc>Shows the current status of local repos</bitbar.desc>

if [ "$1" = "copy" ]; then
    echo "$2" | pbcopy
fi

if [ "$1" = "run" ]; then
    V_MY_PATH=$HOME
    REPO_DIR="$V_MY_PATH/Documents/MacBarPlugins/Files"
    if [ ! -d "$REPO_DIR" ]; then
        mkdir "$REPO_DIR"
    fi
    find "$V_MY_PATH/Documents" -name .git -type d -prune -not -path '*/.*/*' > "$V_MY_PATH/Documents/MacBarPlugins/Files/repos.txt"
fi
