#!/bin/sh
if [[ "$1" = "copy" ]]; then
    echo "$2" | pbcopy
fi

if [[ "$1" = "run" ]]; then
    V_MY_PATH=$HOME
    find "$V_MY_PATH/Documents" -name .git -type d -prune -not -path '*/.*/*' > "$V_MY_PATH/Documents/MacBarPlugins/Files/repos.txt"
fi



