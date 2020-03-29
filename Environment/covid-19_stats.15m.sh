#!/bin/zsh 

# <bitbar.title>COVID-19 Stats</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Wilson Goode</bitbar.author>
# <bitbar.author.github>wilsongoode</bitbar.author.github>
# <bitbar.desc>Displays stats of US COVID-19 cases, with a submenu for user-defineable States. Can also be configured to show the top n states.</bitbar.desc>
# <bitbar.image>https://github.com/wilsongoode/covid-bitbar/blob/master/screens/covid-19_top15_states.png</bitbar.image>
# <bitbar.dependencies>node, npm, corona-cli</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/wilsongoode/covid-bitbar</bitbar.abouturl>


# Setting my Bitbar path to include /usr/local/bin. Systems may vary
PATH=/usr/local/bin:${PATH}
export PATH

# ==============================DEPENDENCIES================================= #
# This script requires Node.js/npm and the following CLI tool                 #
# Requires: https://github.com/ahmadawais/corona-cli                          #
# Install via npm:  `npm i -g corona-cli`                                     #
# =========================================================================== #


# ==============================CONFIGURATION================================ #
# Set these variables to configure the output to your                         #
# liking. Set the directory for your Bitbar Plugins / directory you           #
# want to keep a cache in.                                                    #
BITBAR_DIR=~/Code/bitbar_plugins                                              
                                                                             
# Choose which states you want stats for. Any states you add here will        #
# be shown within the dropdown menu. Be sure to separate each state in        #
# its own parentheses.                                                        #
STATES=("North Carolina" "New York" "California")                             
                                                                             
# ALTERNATIVE MODE: Instead of choosing states, you can choose to have        #
# the top n states. If TOP_N=true, shows N_STATES number of states with       #
# the most cases.                                                             #
TOP_N=true                                                                   
N_STATES=15                                                                   
# =========================================================================== #


# ==============================SCRIPT BELOW================================= #
# This section modifies the call to grep to switch between specific states or 
# TOP_N states.                                                     
MOD_GREP_STATES="#"
if [[ $TOP_N = "true" ]]; then
    MOD_GREP_A="-A$N_STATES"
else
    MOD_GREP_A=""
    for state in "${STATES[@]}"
        do 
            MOD_GREP_STATES="$MOD_GREP_STATES\\|$state"
        done
fi 

# These calls pull data for USA and then individual states, storing in a cache.
corona usa -x -m &>$BITBAR_DIR/.corona_usa_cache
corona states -x -m &>$BITBAR_DIR/.corona_states_cache

# Defining ANSI colors for output
RED='\033[01;31m'
GREEN='\033[01;32m'
YELLOW='\033[01;33m'
BLUE='\033[01;36m'
NONE='\033[0m'

# Top line for USA data
cat $BITBAR_DIR/.corona_usa_cache |
    grep "USA" |
    sed -E 's/[[:space:]][[:space:]][[:space:]]*/;/g' |
    awk -v r=$RED -v y=$YELLOW -v g=$GREEN -v b=$BLUE -v n=$NONE -F';' \
        '{ printf "%15s %15s %15s %15s %15s |font=AndaleMono\n",
            n$2, b"😷"$3, g"("$4"▲)", r"💀"$5, y"("$6"▲)"}'
echo "---"

# Submenu for States of Interest
cat $BITBAR_DIR/.corona_states_cache | 
    grep $MOD_GREP_A $MOD_GREP_STATES |
    sed -E 's/[[:space:]][[:space:]][[:space:]]*/;/g ; 
        s/District Of Columbia/Washington, D.C./ ;
        s/United States Virgin Islands/US Virgin Islands/ ;
        s/Diamond Princess Cruise/Diamond Princess Cr./' |
    awk -v r=$RED -v y=$YELLOW -v g=$GREEN -v b=$BLUE -v n=$NONE -F';' \
        '{ printf "%-30s %20s %30s %20s %30s |font=AndaleMono size=12\n",
            y$2, b$3, g"("$4"▲)", r$5, y"("$6"▲)"}'
