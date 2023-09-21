#!/usr/bin/env bash

# <xbar.title>Battery Estimator</xbar.title>
# <xbar.version>v1.0</xbar.version>
# <xbar.author>Lakeland</xbar.author>
# <xbar.author.github>Lakeland97</xbar.author.github>
# <xbar.desc>Estimates when the battery will run out based on remaining time.</xbar.desc>
# <xbar.dependencies>bash</xbar.dependencies>
# <xbar.image>https://user-images.githubusercontent.com/5545555/269747616-eb28faa6-e37a-4b1c-9842-b2c6fc3cf12f.png</xbar.image>

# Function to get remaining time in HH:MM format
get_remaining_time() {
    pmset -g ps | awk -F ';' '/remaining/ {print $3}' | awk -F ' ' '{print $1}'
}

# Function to get charging status
get_charging_status() {
    pmset -g ps | awk -F ';' '/AC Power/ {print "AC"}; /Battery Power/ {print "Battery"}'
}

# Extract remaining time and charging status
remaining_time=$(get_remaining_time)
charging_status=$(get_charging_status)

if [[ ! -z "$remaining_time" ]]; then
    # Calculate the time of day when the battery will run out
    IFS=":" read -ra TIME <<< "$remaining_time"
    remaining_seconds=$((10#${TIME[0]} * 3600 + 10#${TIME[1]} * 60))
    current_time=$(date +%s)
    end_time_seconds=$((current_time + remaining_seconds))
    
    # Determine the appropriate emoji based on charging status
    #if [[ "$charging_status" == "AC" ]]; then
    #    emoji="🔋"
    #else
    #    emoji="🪫"
    #fi

    # Lightning bolt looks better than a red battery
    emoji="⚡️"


    if (( remaining_seconds >= 86400 )); then
        days=$(( remaining_seconds / 86400 ))
        remaining_seconds=$(( remaining_seconds % 86400 ))
        end_time=$(date -j -f "%s" "$((current_time + remaining_seconds))" +%I:%M\ %p)
        echo "$emoji $days d, $end_time"
    else
        end_time=$(date -j -f "%s" "$end_time_seconds" +%I:%M\ %p)
        echo "$emoji $end_time"
    fi
else
    echo "⚡ --:--"
fi
