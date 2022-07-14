#!/usr/bin/env bash
#  <xbar.title>chrome tabs counter</xbar.title>
#  <xbar.version>v1.0</xbar.version>
#  <xbar.author>Tom Matthews</xbar.author>
#  <xbar.author.github>tomtastic</xbar.author.github>
#  <xbar.desc>Counts the number of open Chrome tabs.</xbar.desc>
#  <xbar.dependencies>osascript</xbar.dependencies>

# Number of tabs open, and the accompanying message color...
acceptable=10
acceptable_color="#2074D5" #sky
worrying=25
worrying_color="#FF8500" #smile
alarming_color="#DF2A5D" #cosmos

script='
if application "Google Chrome" is running then
    tell application "Google Chrome"
        set windowlist to every window
        set tabcount to 0
        set totaltabcount to 0
        set results to ""
        repeat with chromewindow in windowlist
            try
                set tabcount to number of tabs in chromewindow
                set totaltabcount to totaltabcount + tabcount
            on error errmsg
                -- log "error message: " & errmsg
            end try
            set results to results & tabcount & " "
        end repeat
        set results to results & totaltabcount
        log results
    end tell
else
    log "0"
end if
'

# Lets pop the results in an array
read -r -a output <<< "$(/usr/bin/osascript -e "$script" 2>&1)"

num_windows=${#output[@]}-1

# Last item has total tabs value
if [[ "${output[$num_windows]}" -le $acceptable ]]; then
    color=$acceptable_color
elif [[ "${output[$num_windows]}" -le $worrying ]]; then
    color=$worrying_color
else
    color=$alarming_color
fi

echo "🐿 ${output[$num_windows]} Chrome tabs open | color=$color"
echo "---"
for (( i=0; i<num_windows; i++ )); do
    echo "🪟 $(( i+1 )) - has ${output[$i]} tabs"
done
