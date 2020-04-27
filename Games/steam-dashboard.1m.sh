#!/bin/bash

# <bitbar.title>Steam Dashboard</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>naezith</bitbar.author>
# <bitbar.author.github>naezith</bitbar.author.github>
# <bitbar.desc>Display online player count and track daily/weekly/monthly reviews of a Steam game. Please edit parameters.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/hd7dRju.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://naezith.com/</bitbar.abouturl>

# Parameters
APP_ID="590590" # From Steam page like https://store.steampowered.com/app/590590
REVIEW_STATS="all" # Alternatively, put "steam" for only Steam purchases
CACHE_FOLDER="/Users/naezith/Documents/BitBar/naezith_cache" # Previous days will be saved here

# Include /usr/local/bin apps
PATH=/usr/local/bin:${PATH}
export PATH

# Unicode support 
LANG=en_US.UTF-8 
export LANG

# ANSI colors for output with awk
BLACK='\033[00;30m'
RED='\033[00;31m'
GREEN='\033[00;32m'
YELLOW='\033[00;33m'
BLUE='\033[00;34m'
MAGENTA='\033[00;35m'
CYAN='\033[00;36m'
WHITE='\033[00;37m'
NONE='\033[0m'

# Logs path
logs=$CACHE_FOLDER

# Create the folder if it does not exist
[ ! -d $logs ] && mkdir -p $logs

# Fetch store page
store_info=$(curl -s https://store.steampowered.com/api/appdetails\?json=1\&appids=$APP_ID | jq ".[\"$APP_ID\"].data")
game_name=$(echo "$store_info" | jq '.name' | sed 's/\"//g')
website=$(echo "$store_info" | jq '.website' | sed 's/\"//g')
header_image_url=$(echo "$store_info" | jq '.header_image' | cut -d "\"" -f2)
picture=$(curl -s "$header_image_url" | openssl base64 -A)

# Fetch online count
online_count=$(curl -s https://api.steampowered.com/ISteamUserStats/GetNumberOfCurrentPlayers/v1/\?\&appid=$APP_ID | jq '.response.player_count')

# Fetch all reviews, Steam and Key activations for texts
review_info_all=$(curl -s https://store.steampowered.com/appreviews/$APP_ID\?json=1\&start_offset=0\&day_range=9223372036854775807\&language=all\&filter=recent\&num_per_page=100\&purchase_type=all\&review_type=all)
reviews=$(echo "$review_info_all" | jq '.reviews')

# Fetch only Steam purchases for stats
review_info=$(curl -s https://store.steampowered.com/appreviews/$APP_ID\?json=1\&start_offset=0\&day_range=9223372036854775807\&language=all\&filter=recent\&num_per_page=20\&purchase_type=$REVIEW_STATS\&review_type=all)
query_summary=$(echo "$review_info" | jq '.query_summary')
review_score_desc=$(echo "$query_summary" | jq '.review_score_desc')
total_positive=$(echo "$query_summary" | jq '.total_positive')
total_negative=$(echo "$query_summary" | jq '.total_negative')
total_reviews=$(echo "$query_summary" | jq '.total_reviews')

# Prepare dates
today=$(date +%F)
yesterday=$(date -v-1d +%F)
last_week=$(date -v-1w +%F)
last_month=$(date -v-1m +%F)
yesterday_file=$logs/$yesterday
last_week_file=$logs/$last_week
last_month_file=$logs/$last_month

# Save today's data
echo "$total_reviews" "$total_positive" "$total_negative" > $logs/"$today"

# Function: Get review count from file
get_review_count () { 
    cut -d ' ' -f1 < "$1"
}

# Function Get review count difference from file
get_review_diff () { 
    pl=$(get_review_count "$1")
    echo "$total_reviews-$pl" | bc
}

# Get differences
[ -r "$yesterday_file" ] && r_yesterday_diff=$(get_review_diff "$yesterday_file") || r_yesterday_diff=0 # Shows at taskbar so set it to zero if there is no data
[ -r "$last_week_file" ] && r_last_week_diff=$(get_review_diff "$last_week_file")
[ -r "$last_month_file" ] && r_last_month_diff=$(get_review_diff "$last_month_file")

# Menu bar
echo "$online_count" $r_yesterday_diff | awk -v "bl=$BLACK" -v "w=$WHITE" -v "y=$YELLOW" -v "b=$BLUE" -v "c=$CYAN" -v "g=$GREEN" -v "m=$MAGENTA" -v "n=$NONE" '{printf g"🟢"$1 bl"⁃⁃" m"✍🏻"$2n}'
    
        
# Dropdown Menu
echo -e "\n---"

# Title
echo "${game_name} | href=$website" 

# Seperator
echo "---"

# Player info
[ "$online_count" -ne 0 ] && echo "🟢" "$online_count" players are currently online! || echo "🟢" There isn\'t any online player 

# Seperator
echo "---"

# Review Stats
reviews_url="| href=https://store.steampowered.com/app/$APP_ID/#app_reviews_hash" 
review_perc=$(awk -v a="$total_positive" -v b="$total_reviews" 'BEGIN{printf("%.2f\n",100*a/b)}')
echo "✍🏻" "$review_perc"%, "${review_score_desc//\"/}" "$total_reviews" reviews "$reviews_url"
echo "👍🏻" "$total_positive" positive and "👎🏻" "$total_negative" negative "$reviews_url"

# Recent reviews
for review in $(echo "${reviews}" | jq -r '.[] | @base64')
do
    _jq() { 
        echo "${review}" | base64 --decode | jq -r "${1}"
    }

    # Print reviews
    mins=$(_jq '.author.playtime_forever') 
    mins_l2w=$(_jq '.author.playtime_last_two_weeks') 
    voted_up=$(_jq '.voted_up')
    steam_purchase=$(_jq '.steam_purchase')
    language=$(_jq '.language')
    timestamp_created=$(_jq '.timestamp_created')

    # Purchase type
    if [ "$steam_purchase" == "true" ] ; then
        t_activation="Steam Purchase"
        c_activation=$BLUE
    else
        t_activation="Key Activation"
        c_activation=$YELLOW
    fi
    
    # Vote
    if [ "$voted_up" == "true" ] ; then
        t_vote="👍🏻"
        c_date=$GREEN
    else
        t_vote="👎🏻"
        c_date=$RED
    fi

    # Other values
    t_date=$(date -r "$timestamp_created" | cut -d ' ' -f-4 | cut -d ":" -f-2)
    t_play_time=$(printf 'Playtime: %dh %dm\n' $((mins/60)) $((mins%60)))
    t_l2w=$(printf 'Last two weeks: %dh %dm\n' $((mins_l2w/60)) $((mins_l2w%60)))
    t_lang=$(tr '[:lower:]' '[:upper:]' <<< "${language:0:1}")${language:1}
    t_href=" | href=https://steamcommunity.com/profiles/$(_jq '.author.steamid')/recommended/$APP_ID/"
    t_review=$(_jq '.review' | sed -e ':a' -e 'N' -e '$!ba' -e 's/\n/ /g')
    
    printf "%-2s %-20s ${c_date}%-40s${NONE} ${c_activation}%-40s${NONE} ${MAGENTA}%-40s${NONE} ${BLUE}%-50s${NONE} ${WHITE}%s${NONE} %s\n" '--' "$t_vote" "$t_date" "$t_activation" "$t_play_time" "$t_l2w" "$t_lang" "$t_href"
    printf "%-2s %s\n" '--' "$t_review"
    echo --"---"
done

echo "---"

# Show Today
if [ -z "$r_yesterday_diff" ] ; then
    echo "🌝" There isn\'t reviews data for yesterday
else 
    [ $r_yesterday_diff -ne 0 ] && echo "🌝" $r_yesterday_diff new reviews today! || echo "🌝" No new reviews today
fi

# Show This week
if [ -z "$r_last_week_diff" ] ; then
    echo "🌓" There isn\'t reviews data for last week
else 
    [ "$r_last_week_diff" -ne 0 ] && echo "🌓" "$r_last_week_diff" new reviews this week! || echo "🌓" No new reviews this week
fi

# Show This month
if [ -z "$r_last_month_diff" ] ; then
    echo "🌑" There isn\'t reviews data for last month
else 
    [ "$r_last_month_diff" -ne 0 ] && echo "🌑" "$r_last_month_diff" new reviews this month! || echo "🌑" No new reviews this month
fi

# Seperator
echo "---"

# Remnants of Naezith
echo "| image=$picture href=https://store.steampowered.com/app/$APP_ID"

# Seperator
echo "---"

# Refresh button
echo "Refresh | refresh=true"
