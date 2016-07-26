#!/bin/bash
# <bitbar.title>CrossFit Regionals</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Dean Fogarty</bitbar.author>
# <bitbar.author.github>angrytongan</bitbar.author.github>
# <bitbar.desc>Display top 3 results from all CrossFit regionals in current year.</bitbar.desc>
# <bitbar.dependencies>bash</bitbar.dependencies>

YEAR=$(date +%Y | sed -e "s/.*\(..\)$/\1/")

declare -a regions
regions[1]="Atlantic"
regions[2]="California"
regions[3]="Central"
regions[4]="East"
regions[5]="Meridian"
regions[6]="Pacific"
regions[7]="South"
regions[8]="West"

echo "Regionals"
echo "---"

for region in "${!regions[@]}"; do
    echo ${regions[$region]}

    curl -s http://games.crossfit.com/scores/leaderboard.php?year="${YEAR}"\&division=201\&regional="${region}"\&numberperpage=3 | grep 'games.crossfit.com/athlete' | sed "s/.*_top\">\(.*\)<\/a><\/td>/-- \\1/g"
    echo "--"

    curl -s http://games.crossfit.com/scores/leaderboard.php?year="${YEAR}"\&division=101\&regional="${region}"\&numberperpage=3 | grep 'games.crossfit.com/athlete' | sed "s/.*_top\">\(.*\)<\/a><\/td>/-- \\1/g"
    echo "--"

    curl -s http://games.crossfit.com/scores/leaderboard.php?year="${YEAR}"\&division=301\&regional="${region}"\&numberperpage=3 | grep 'games.crossfit.com/team' | sed "s/.*_top\">\(.*\)<\/a><\/td>/-- \\1/g"

done
