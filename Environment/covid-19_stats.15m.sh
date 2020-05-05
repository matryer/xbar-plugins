#!/bin/bash

# <bitbar.title>COVID-19 Stats</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Wilson Goode</bitbar.author>
# <bitbar.author.github>wilsongoode</bitbar.author.github>
# <bitbar.desc>Displays stats of US COVID-19 cases, with a submenu for user-defineable States. Can also be configured to show the top n states.</bitbar.desc>
# <bitbar.image>https://github.com/wilsongoode/covid-bitbar/raw/master/screens/covid-19_top15_states.png</bitbar.image>
# <bitbar.dependencies>jq</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/wilsongoode/covid-bitbar</bitbar.abouturl>

# Setting my Bitbar path to include /usr/local/bin. Systems may vary
PATH=/usr/local/bin:${PATH}
export PATH
LANG=en_US.UTF-8 # needed in BitBar env for awk to format numbers
export LANG

# ==============================DEPENDENCIES=================================
# This script requires jq for manipulating JSON data.
# Requires: https://stedolan.github.io/jq/
# Install via brew:  `brew install jq`
# ===========================================================================

# ===============================DATA SOURCE=================================
# This script curls JSON data from the NovelCOVID API, available at:
# https://corona.lmao.ninja
# GitHub: https://github.com/novelcovid/api
# There are a few other APIs or tools for pulling COVID-19 data, several of
# which point back to this one. v1.0 of this script used a node.js-based CLI
# to pull data, leading to formatting issues across systems and some
# inconsistencies in iTerm/Terminal and in the BitBar output.
# ===========================================================================


# ==============================CONFIGURATION================================
# READ THIS SECTION:
# Set these variables to configure the output to your liking.
#
# Choose which states you want stats for. Any states you add here will
# be shown within the dropdown menu. Be sure to separate each state in
# its own parentheses with a space between each string.
# Example:
# STATES=("North Carolina" "New York" "California")
STATES=("North Carolina" "New York" "California")
#
# ALTERNATIVE TOP STATES MODE:
# Instead of choosing states, you can choose to have the top n states ranked
# by number of cases, where n is a number you set with the variable N_STATES.
# Comment/uncomment one of the next two lines to set your preference.
TOP_N=true     # Sets the script to show the top states
# TOP_N=false    # Sets the script to show user-selected states
#
# Set the number of states you want to see when TOP_N is true. The data
# source has 57 entries, including Puerto Rice, US Virgin Islands, and the
# Diamond Princess cruise. In case the maintainers add more entries, set the
# number to something much higher (it won't effect performance)
N_STATES=500
# ===========================================================================
# ==============================SCRIPT BELOW=================================


# Modifies settings based on TOP_N configuration
if [[ $TOP_N = "true" ]]; then
    MOD_STATES="$" # Select every line
    GREP_LIMIT="-m$((2 + N_STATES))" # Off-by-2 due to header and divider
    TOP_CONFIG="Showing the top $N_STATES states"
else
    MOD_STATES="State\|---" # Select header and divider
    GREP_LIMIT="" # No limit
    TOP_CONFIG="Showing user-selected states"
    for state in "${STATES[@]}"
        # Adds each user-selected state to the grep call
        do
            MOD_STATES="$MOD_STATES\\|$state"
        done
fi

# Setting ANSI colors for output with awk.
RED='\033[01;31m'
GREEN='\033[01;32m'
YELLOW='\033[01;33m'
BLUE='\033[01;36m'
NONE='\033[0m'

# USA data for the menu bar line
curl -s https://corona.lmao.ninja/countries/USA |
    # Manipulates data and exports tab-delimited (tsv)
    jq -r '. | [.country, .cases, .todayCases, .deaths, .todayDeaths] | @tsv' |
    # Removes quotes
    sed -E 's/"//g' |
    # Prints numbers with comma as thousands place separators
    awk -F'\t' '{ printf "%s\t%\047d\t%\047d\t%\047d\t%\047d\n",
        $1, $2, $3, $4, $5 }' |
    # Pretty-prints with colors and spacing and emojis
    awk -v "r=$RED" -v "y=$YELLOW" -v "g=$GREEN" -v "b=$BLUE" -v "n=$NONE" -F'\t' \
        '{ printf ("%15s %15s %15s %15s %15s |font=AndaleMono\n",
            n$1, "😷"b$2, g"("$3"▲)", "💀"r$4, y"("$5"▲)") }'
echo "---"

# STATES data for the submenu
# HINT: you can change the sort order to one of the following:
# cases, todayCases, deaths, todayDeaths, recovered, active, critical,
# casesPerOneMillion, deathsPerOneMillion
# As the data source updates, it is possible more sort options will be added
curl -s https://corona.lmao.ninja/states\?sort=cases |
    # Manipulates data and exports tab-delimited (tsv)
    jq -r '["State", "Cases", "Cases (today)", "Deaths", "Deaths (today)"],
        ["---"],
        (.[] | [.state, .cases, .todayCases, .deaths, .todayDeaths]) | @tsv' |
    # Removes quotes, shortens a few of the longer names
    sed -E 's/"//g;
        s/District Of Columbia/Washington D.C./;
        s/Northern Mariana Islands/N. Mariana Islands/;
        s/United States Virgin Islands/US Virgin Islands/;
        s/Diamond Princess Cruise/Diamond Princess Cr./' |
    # Grabs specific states/lines or every line, depending on configuration
    grep $GREP_LIMIT "$MOD_STATES" |
    # Prints numbers with comma as thousands place separators
    awk -F'\t' '{ if ($0 ~ "State") { print $0 } else
        if ($0 ~ "---") { print $0 } else
        { printf "%s\t%\047d\t%\047d\t%\047d\t%\047d\n",
            $1, $2, $3, $4, $5 } }' |
    # Pretty-prints with colors and spacing
    awk -v "r=$RED" -v "y=$YELLOW" -v "g=$GREEN" -v "b=$BLUE" -v "n=$NONE" -F'\t' \
        '{if ($0 ~ "---" ) { print $0 } else
        { printf "%-30s %20s %30s %20s %30s |font=AndaleMono size=12\n",
            y$1, b$2, g$3"▲", r$4, y$5"▲" }}'
echo "---"

# WORLD totals for the submenu
curl -s https://corona.lmao.ninja/all |
    # Manipulates data and exports tab-delimited (tsv)
    jq -r '. | ["World", .cases, .todayCases, .deaths, .todayDeaths] | @tsv' |
    # Removes quotes
    sed -E 's/"//g' |
    # Prints numbers with comma as thousands place separators
    awk -F'\t' '{ printf "%s\t%\047d\t%\047d\t%\047d\t%\047d\n",
        $1, $2, $3, $4, $5 }' |
    # Pretty-prints with colors and spacing
    awk -v "r=$RED" -v "y=$YELLOW" -v "g=$GREEN" -v "b=$BLUE" -v "n=$NONE" -F'\t' \
        '{ printf "%-30s %20s %30s %20s %30s |font=AndaleMono size=12\n",
            y$1, b$2, g$3"▲", r$4, y$5"▲"}'
echo "---"
echo "Configuration: $TOP_CONFIG"
echo "Refresh | refresh=true"
