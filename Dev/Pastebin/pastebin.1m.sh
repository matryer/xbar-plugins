#!/bin/bash

#
# <bitbar.title>Check Pastebin For New Pastes</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Tyllis Xu</bitbar.author>
# <bitbar.author.github>LivelyCarpet87</bitbar.author.github>
# <bitbar.desc>Uses a set of Pastebin API keys to check for pastes created by the user. It will provide links to all the pastes it finds by the user. </bitbar.desc>
# <bitbar.image></bitbar.image>
# <bitbar.abouturl></bitbar.abouturl>
#

#Requires Configuration Here

#Pastebin developer key
dev_key=""
#Pastebin user key
usr_key=""

#Constants
#Pastebin API Get_Paste URL
get_paste_url="https://pastebin.com/api/api_post.php"
#empty pastes arrays
pasteNameArr=()
pasteLinkArr=()

queryResults=$(curl --silent -X POST -d "api_option=list&api_user_key=$usr_key&api_dev_key=$dev_key" $get_paste_url)

titles=$(echo "$queryResults" | egrep "<paste_title>(\S*)<\/paste_title>" --context=0 | sed s+\<paste_title\>\<\/paste_title\>+Untitled+g | sed s+\<paste_title\>++g | sed s+\<\/paste_title\>++g)

pasteURLs=$(echo "$queryResults" | egrep "<paste_url>(\S*)<\/paste_url>" --context=0 | sed s+\<paste_url\>++g | sed s+\<\/paste_url\>++g)
SAVEIFS=$IFS
IFS=$'\n' 
pasteNameArr=($titles)
pasteLinkArr=($pasteURLs)
IFS=$SAVEIFS 

totalPastes=$((${#pasteNameArr[@]}))

echo Pastes Found: $totalPastes

i=0

while [[ $i < $totalPastes ]]
do
printf '%s\n' "Paste $(($i + 1)): ${pasteNameArr[$i]}"
i=$(($i + 1))
done

echo "---"

i=0
while [[ $i < $totalPastes ]]
do


printf '%s\n' " Paste $(($i + 1)) | href=${pasteLinkArr[$i]}"
i=$(($i + 1))
done