#!/bin/bash

# <bitbar.title>Clock with calendar</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Weibing Chen</bitbar.author>
# <bitbar.author.github>WeibingChen17</bitbar.author.github>
# <bitbar.desc>A clock with a simple calendar</bitbar.desc>
# <bitbar.image>https://i.ibb.co/Krmg0P8/Screen-Shot-2019-11-08-at-23-04-29.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/WeibingChen17/</bitbar.abouturl>

# If using alongside apples default clock, one can uncomment the following - 
#formatted_date=$(date '+%d/%b/%Y')
#printf '\xF0\x9F\x93\x85 %s\n' "$formatted_date"
# don't forget to comment out the following `date ` command:
date "+%l:%M %p"

echo "---"
font="Monaco"
color="red"

#Uncomment the below line and comment out all other following lines to trigger the three-month mode
#cal - 3 |awk 'NF'|sed 's/ $//' |while IFS= read -r i; do echo " $i|trim=false font=$font color=$color"|  perl -pe '$b="\b";s/ _$b(\d)_$b(\d) /(\1\2)/' |perl -pe '$b="\b";s/_$b _$b(\d) /(\1)/'  ; done

#Comment out these lines to remove "last month"
last_month=$(date -v-1m +%m)
last_year=$(date -v-1m +%Y)
last_month_name=$(date -jf %Y-%m-%d "$last_year"-"$last_month"-01 '+%b')
echo "Prev: $last_month_name $last_year|trim=false font=$font"
cal -d "$last_year"-"$last_month" |awk 'NF'|sed 's/ *$//'| while IFS= read -r i; do echo "--$i|trim=false font=$font"; done 
echo "---"

#cal |awk 'NF'|while IFS= read -r i; do echo " $i|trim=false font=$font color=$color"|  perl -pe '$b="\b";s/ _$b(\d)_$b(\d) /(\1\2)/' |perl -pe '$b="\b";s/_$b _$b(\d) /(\1)/' |sed 's/ *$//'; done 
cal |awk 'NF'|while IFS= read -r i; do echo " $i"|perl -pe '$b="\b";s/ _$b(\d)_$b(\d) /(\1\2)/' |perl -pe '$b="\b";s/_$b _$b(\d) /(\1)/' |sed 's/ *$//' |sed "s/$/|trim=false font=$font color=$color/"; done 

#Comment out these lines to remove "next month"
echo "---"
next_month=$(date -v+1m +%m)
next_year=$(date -v+1m +%Y)
next_month_name=$(date -jf %Y-%m-%d "$next_year"-"$next_month"-01 '+%b')
echo "Next: $next_month_name $next_year|trim=false font=$font"
cal -d "$next_year"-"$next_month" | awk 'NF'|sed 's/ *$//' | while IFS= read -r i; do echo "--$i|trim=false font=$font";done
