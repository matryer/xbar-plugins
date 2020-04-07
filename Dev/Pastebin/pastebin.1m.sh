#!/bin/bash

#
# <bitbar.title>Check Pastebin</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tyllis Xu</bitbar.author>
# <bitbar.author.github>LivelyCarpet87</bitbar.author.github>
# <bitbar.desc>Uses a set of Pastebin API keys to check for pastes created by the user. It will provide links to all the pastes it finds by the user. </bitbar.desc>
# <bitbar.image>https://i.ibb.co/wJM47wp/pastebin.png</bitbar.image>
# <bitbar.abouturl></bitbar.abouturl>
#

#Requires Configuration Here

#Pastebin developer key
dev_key=""
#Pastebin user key
usr_key=""


#Constants
#Pastebin API List Paste URL
list_paste_url="https://pastebin.com/api/api_post.php"
#Pastebin API Get Paste URL
get_paste_url="https://pastebin.com/api/api_raw.php"


#empty pastes arrays
pasteNameArr=()
pasteLinkArr=()

queryResults=$(curl --silent --connect-timeout 15 --speed-time 15 --speed-limit 500  -X POST -d "api_option=list&api_user_key=$usr_key&api_dev_key=$dev_key" $list_paste_url)

titles=$(echo "$queryResults" | grep -E "<paste_title>([^\r]*)" --context=0 | sed s+'<paste_title></paste_title>'+Untitled+g | sed s+\<paste_title\>++g | sed s+'</paste_title>'++g |tr '\r' ' ')

pasteURLs=$(echo "$queryResults" | grep -E "<paste_key>(\S*)</paste_key>" --context=0 | sed s+\<paste_key\>++g | sed s+'</paste_key>'++g |tr '\r' ' ')
SAVEIFS=$IFS
IFS=$'\n\r'
#shellcheck disable=SC2206
pasteNameArr=($titles)
#shellcheck disable=SC2206
pasteLinkArr=($pasteURLs)
IFS=$SAVEIFS

totalPastes=$((${#pasteNameArr[@]}))



# test if the request response was valid
if [[ $(echo "queryResults" | grep "Bad API Request" -c) == 0 ]]
then

echo Pastes Found: $totalPastes

i=0
while [[ $i < $totalPastes ]]
do
printf '%s\n' "$((i + 1)): ${pasteNameArr[$i]} | length=15 dropdown=false"
i=$((i + 1))
done

echo "---"
i=0
while [[ $i < $totalPastes ]]
do
printf '%s\n' "Paste $((i + 1)): ${pasteNameArr[$i]} |href=https://pastebin.com/${pasteLinkArr[$i]}"
echo -- "$(curl --silent -X POST --connect-timeout 15 --speed-time 15 --speed-limit 500 -d "api_option=show_paste&api_user_key=$usr_key&api_dev_key=$dev_key&api_paste_key=${pasteLinkArr[$i]}"  $get_paste_url)"
i=$((i + 1))
done

# test if it is a connectivity issue
elif [[ $(curl https://pastebin.com/api/api_post.php --silent --connect-timeout 15 --speed-time 15 --speed-limit 500| grep "Bad API request" -c) == 0  ]]
then

echo "Disconnected | color=yellow"
echo "---"
echo "Refresh Now | refresh=true color=blue"

else

echo "Bad API Request | color=red"
echo Please check keys for errors
echo "---"
echo "Refresh Now | refresh=true color=blue"

fi
