#!/usr/bin/env bash
# <xbar.title>Tabs Counter</xbar.title>
# <xbar.version>v2.0</xbar.version>
# <xbar.author>Tom Matthews</xbar.author>
# <xbar.author.github>tomtastic</xbar.author.github>
# <xbar.desc>Counts the number of open Safari and/or Chrome tabs, and the memory used by both</xbar.desc>
# <xbar.image>https://www.dropbox.com/s/0scul4sv8jtw4ev/counttabs.jpg?raw=1</xbar.image>
# <xbar.dependencies>osascript</xbar.dependencies>

set -euo pipefail

acceptable=10
acceptable_color="#2074D5" # sky
worrying=25
worrying_color="#FF8500" # smile
alarming_color="#DF2A5D" # cosmos

get_tab_counts() {
    local app_name=$1

    /usr/bin/osascript -e "if application \"$app_name\" is running then
        tell application \"$app_name\"
            set windowlist to every window
            set tabcount to 0
            set totaltabcount to 0
            set results to \"\"
            repeat with browserwindow in windowlist
                try
                    set tabcount to number of tabs in browserwindow
                    set totaltabcount to totaltabcount + tabcount
                on error errmsg
                    set tabcount to 0
                end try
                set results to results & tabcount & \" \"
            end repeat
            set results to results & totaltabcount
            return results
        end tell
    else
        return \"0\"
    end if" 2>/dev/null || echo "0"
}

get_mem_size() {
    local process_name=$1
    local rss

    rss=$({ ps -eo rss,comm 2>/dev/null || true; } | awk -v process_name="$process_name" '
        index($0, process_name) {
            total += $1
        }
        END {
            printf "%.0f", total / 1024
        }
    ')

    echo "${rss:-0}"
}

browser_running() {
    local app_name=$1
    [[ "$(/usr/bin/osascript -e "application \"$app_name\" is running" 2>/dev/null || true)" == "true" ]]
}

print_window_rows() {
    local app_name=$1
    local process_name=$2
    shift 2
    local counts=("$@")
    local num_windows=${#counts[@]}

    for ((i = 0; i < num_windows; i++)); do
        echo "🪟 $((i + 1)) - has ${counts[$i]} tabs | shell=/usr/bin/osascript param1='-e' param2='tell Application \"System Events\" to tell process \"$process_name\" to perform action \"AXRaise\" of window $((i + 1))' param3='-e' param4='tell Application \"$app_name\" to activate'"
    done
}

read -r -a safari_output <<< "$(get_tab_counts "Safari")"
read -r -a chrome_output <<< "$(get_tab_counts "Google Chrome")"

safari_total_index=$((${#safari_output[@]} - 1))
chrome_total_index=$((${#chrome_output[@]} - 1))
safari_total=${safari_output[$safari_total_index]}
chrome_total=${chrome_output[$chrome_total_index]}
safari_windows=("${safari_output[@]:0:$safari_total_index}")
chrome_windows=("${chrome_output[@]:0:$chrome_total_index}")
safari_mem=$(get_mem_size "Safari.app")
chrome_mem=$(get_mem_size "Google Chrome.app")
total_tabs=$((safari_total + chrome_total))
total_mem=$((safari_mem + chrome_mem))

if [[ "$total_tabs" -le $acceptable ]]; then
    color=$acceptable_color
elif [[ "$total_tabs" -le $worrying ]]; then
    color=$worrying_color
else
    color=$alarming_color
fi

echo "🐿 $total_tabs tabs | color=$color"
echo "---"
echo "Safari: $safari_total tabs, ${#safari_windows[@]} windows"
echo "Chrome: $chrome_total tabs, ${#chrome_windows[@]} windows"

if browser_running "Safari" && ((${#safari_windows[@]} > 0)); then
    echo "---"
    echo "Safari"
    print_window_rows "Safari" "Safari" "${safari_windows[@]}"
fi

if browser_running "Google Chrome" && ((${#chrome_windows[@]} > 0)); then
    echo "---"
    echo "Chrome"
    print_window_rows "Google Chrome" "Google Chrome" "${chrome_windows[@]}"
fi

echo "---"
echo "Memory"
echo "Safari: ${safari_mem} Mb"
echo "Chrome: ${chrome_mem} Mb"
echo "Total: ${total_mem} Mb"
