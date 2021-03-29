#!/bin/bash

# <xbar.title>COVID-19 Today</xbar.title>
# <xbar.version>v1.0.0</xbar.version>
# <xbar.author>CartoonChess</xbar.author>
# <xbar.author.github>cartoonchess</xbar.author.github>
# <xbar.desc>Displays changes in daily and average COVID-19 cases for a given country.</xbar.desc>
# <xbar.image>https://user-images.githubusercontent.com/43363630/93694795-da7d0700-fb4a-11ea-875b-02f29152e929.png</xbar.image>
# <xbar.dependencies>bash,jq</xbar.dependencies>
# <xbar.abouturl>https://github.com/cartoonchess/bitbar-covid-19-today</xbar.abouturl>

# ==============================CONFIGURATION================================
# Set these variables to configure the output to your liking.
# The country must be a two- or three-letter country code.
COUNTRY="kr"
SHOW_CASES_TODAY_BESIDE_ICON=true
# ===========================================================================



# ==============================DEPENDENCIES=================================
# This script requires jq for manipulating JSON data.
# Requires: https://stedolan.github.io/jq/
# Install via brew:  `brew install jq`
# ===========================================================================

# ===============================DATA SOURCE=================================
# This script curls JSON data from disease.sh, the Open Disease API:
# https://disease.sh/
# GitHub: https://github.com/disease-sh/api
# ===========================================================================

# ================================PRIOR ART==================================
# This script is a modified version of:
# covid-bitbar
# https://github.com/wilsongoode/covid-bitbar
# by Wilson Good
# Check there for a version with detailed stats for the US.
# Many thanks!
# ===========================================================================



# Setting my Bitbar path to include /usr/local/bin. Systems may vary
# jq fails without this
PATH=/usr/local/bin:${PATH}
export PATH
LANG=en_US.UTF-8 # needed in BitBar env for awk to format numbers
export LANG



# Test for jq

JSON_PARSER="jq"
if [ ! $(command -v $JSON_PARSER) ]; then
    echo "⚠"
    echo "---"
    echo "$JSON_PARSER Not Installed"
    echo "Install ${JSON_PARSER}… | href=https://stedolan.github.io/${JSON_PARSER}/download/"
    echo "---"
    echo "Refresh | refresh=true"
    exit
fi



# Methods

# Return the singular or plural form of a word based on some count
# Usage:
# COUNT=15
# echo "I have $COUNT $(plural $COUNT 'bee' 'bees')"
# Output: I have 15 bees
plural() {
    COUNT=$1
    SINGULAR=$2
    PLURAL=$3
    if [ $COUNT = 1 ]; then
        echo $SINGULAR
    else
        echo $PLURAL
    fi
}

# Return number with comma as thousands place separator
# Usage:
# BIG_NUM=1500
# echo "I have $(commas $BIG_NUM) bees"
# Output: I have 1,500 bees
commas() {
    echo $(awk 'BEGIN{printf "%\047d\n", '$1'}')
}



# Get the three-letter country code to link to a detailed graph

COUNTRY_DATA=$(curl -s https://disease.sh/v3/covid-19/countries/$COUNTRY)

# Maybe the server is down?
if [ -z "$COUNTRY_DATA" ]; then
    echo "⚠"
    echo "---"
    echo "No Information"
    echo "Refresh | refresh=true"
    exit
fi

COUNTRY_CODE=$(echo $COUNTRY_DATA |
    jq '.countryInfo.iso3' |
    sed -E 's/"//g'
    )

# If there's no country code, there's a problem with the data
# jq returns a literal string "null", NOT a null value
if [ $COUNTRY_CODE = "null" ]; then
    echo "⚠"
    echo "---"
    echo "Invalid Country Code"
    echo "Use a two- or three-letter code in the script"
    echo "Open Plugin Folder… | href=file://${0%/*}/"
    echo "---"
    echo "Refresh | refresh=true"
    exit
fi

    

# Fetch various numbers
# Somehow this data seems more reliable than the data above.
# Namely, it often reports zero new cases when that simply isn't true.

HISTORICAL_DATA=$(curl -s https://disease.sh/v3/covid-19/historical/$COUNTRY\?lastdays=3)
COUNTRY_NAME=$(echo $HISTORICAL_DATA |
    jq '.country' |
    # Remove quotation marks
    sed -E 's/"//g'
    )

CASES_TODAY=$(echo $HISTORICAL_DATA |
    jq '[.timeline.cases[]] | sort | .[-1] - .[-2]'
    )
    
CASES_YESTERDAY=$(echo $HISTORICAL_DATA |
    jq '[.timeline.cases[]] | sort | .[-2] - .[-3]'
    )
    
TOTAL_CASES=$(
    echo $HISTORICAL_DATA |
    jq '[.timeline.cases[]] | sort | .[-1]'
    )
   
   
    
# Calculate daily stats

TODAY=$(date +%s)
# Data begins at 2020-01-22
# Mac-specific formatting; Linux would require a change
DAY_ONE=$(date -jf "%Y-%m-%d" "2020-01-22" +%s)
TOTAL_DAYS=$((($TODAY-$DAY_ONE)/(3600*24)))
DAILY_AVERAGE=$(($TOTAL_CASES/$TOTAL_DAYS))

TODAY_VS_YESTERDAY=$(($CASES_TODAY-$CASES_YESTERDAY))
TODAY_VS_AVERAGE=$(($CASES_TODAY-$DAILY_AVERAGE))



# Determine differences and format strings

# Absolute value of difference between new cases today and yesterday
if [ $TODAY_VS_YESTERDAY = 0 ]; then
    DAILY_DIFFERENCE="▶ No more" # —
elif [ $TODAY_VS_YESTERDAY -lt 0 ]; then
    DAILY_DIFFERENCE=$((-$TODAY_VS_YESTERDAY))
    # awk prints numbers with comma as thousands place separators
    DAILY_DIFFERENCE=$(commas $DAILY_DIFFERENCE)
    DAILY_DIFFERENCE="▼ $DAILY_DIFFERENCE fewer"
else
    DAILY_DIFFERENCE=$(commas $TODAY_VS_YESTERDAY)
    DAILY_DIFFERENCE="▲ $DAILY_DIFFERENCE more"
fi

# Absolute value of difference between total cases today and daily average
if [ $TODAY_VS_AVERAGE = 0 ]; then
    AVERAGE_DIFFERENCE="⦵ No more" # —⃝
elif [ $TODAY_VS_AVERAGE -lt 0 ]; then
    AVERAGE_DIFFERENCE=$((-$TODAY_VS_AVERAGE))
    AVERAGE_DIFFERENCE=$(commas $AVERAGE_DIFFERENCE)
    AVERAGE_DIFFERENCE="—̥ $AVERAGE_DIFFERENCE fewer" # ⏁—̥⃝
else
    AVERAGE_DIFFERENCE=$(commas $TODAY_VS_AVERAGE)
    AVERAGE_DIFFERENCE="—̊ $AVERAGE_DIFFERENCE more" # ⏂—̊⃝
fi



# Show menu bar item
# TODO: What about "break even" icons??

# ⤉⤈⤒⤓ ▼̅▼̲▲̅▲̲ ⤉⃝⤈⃝⤒⃝⤓⃝ ▼̅⃝▼̲⃝▲̅⃝▲̲⃝
if [ $TODAY_VS_YESTERDAY = 0 ]; then
    if [ $TODAY_VS_AVERAGE = 0 ]; then
        ICON="⦵"
    elif [ $TODAY_VS_AVERAGE -lt 0 ]; then
        ICON="—̥"
    else
        ICON="—̊"
    fi
elif [ $TODAY_VS_YESTERDAY -lt 0 ]; then
    if [ $TODAY_VS_AVERAGE = 0 ]; then
        # This and its upward equivalent render without strikethrough without other text
        # i.e. If not showing daily count, only the triangle appears
        ICON="▽̶"
    elif [ $TODAY_VS_AVERAGE -lt 0 ]; then
        ICON="⤈"
    else
        ICON="⤓"
    fi
else
    if [ $TODAY_VS_AVERAGE = 0 ]; then
        ICON="△̶"
    elif [ $TODAY_VS_AVERAGE -lt 0 ]; then
        ICON="⤒"
    else
        ICON="⤉"
    fi
fi

CASES_TODAY_FORMATTED=$(commas $CASES_TODAY)
if [ $SHOW_CASES_TODAY_BESIDE_ICON = true ]; then
    ICON="$ICON $CASES_TODAY_FORMATTED"
fi

echo $ICON
echo "---"



# Show dropdown details

# Total new cases today
# Add "s" for plural, if plural
echo "$CASES_TODAY_FORMATTED new $(plural $CASES_TODAY 'case' 'cases') today in $COUNTRY_NAME | href=https://ourworldindata.org/coronavirus-data-explorer?interval=daily&country=$COUNTRY_CODE"

echo $DAILY_DIFFERENCE" new $(plural $TODAY_VS_YESTERDAY 'case' 'cases') than yesterday"
echo $AVERAGE_DIFFERENCE" daily $(plural $TODAY_VS_AVERAGE 'case' 'cases') than average"



# Options

echo "---"
echo "Update | refresh=true"
echo "Settings"
echo "-- Edit the plugin directly to:"
echo "-- • Change the country"
echo "-- • Show or hide daily cases beside icon"
echo "-----"
echo "-- Open Plugin Folder… | href=file://${0%/*}/"
