#!/bin/bash

#
# <bitbar.title>Check Pastebin</bitbar.title>
# <bitbar.version>v1.2</bitbar.version>
# <bitbar.author>Tyllis Xu</bitbar.author>
# <bitbar.author.github>livelycarpet87</bitbar.author.github>
# <bitbar.desc>Uses a set of Pastebin API keys to check for pastes created by the user. It will provide links to all the pastes it finds by the user. </bitbar.desc>
# <bitbar.image>https://i.ibb.co/cym797V/Pastebin2.png</bitbar.image>
# <bitbar.abouturl>https://github.com/LivelyCarpet87/BitBar-Pastebin#bitbar-pastebin</bitbar.abouturl>
#

#Requires Configuration Here

#Pastebin developer key
dev_key=""

#Pastebin user key
usr_key=""

#Save-To Directory
#Saved pastes will be saved to this directory
saveDir=""

#Enable Deletion of pastes
# 0=disabled | 1=enabled
deleteEnabled=1

#End Configuration

#Constants
#Pastebin API List Paste URL
list_paste_url="https://pastebin.com/api/api_post.php"
#Pastebin API Get Paste URL
get_paste_url="https://pastebin.com/api/api_raw.php"


#empty pastes arrays
pasteNameArr=()
pasteKeyArr=()

#Initialize the temp directory if it does not exist
if [ ! -d /tmp/pastebinReader ]
then
mkdir /tmp/pastebinReader
fi

#clear the temp directory
rm -f /tmp/pastebinReader/*


#get a list of all the pastes

queryResults=$(curl --silent --connect-timeout 15 --speed-time 15 --speed-limit 500  -X POST -d "api_option=list&api_user_key=$usr_key&api_dev_key=$dev_key" $list_paste_url)

#parse the paste titles and keys

titles=$(echo "$queryResults" | grep -E "<paste_title>([^\r]*)" --context=0 | sed s+'<paste_title></paste_title>'+Untitled+g | sed s+\<paste_title\>++g | sed s+'</paste_title>'++g |tr '\r' ' ')

pasteURLs=$(echo "$queryResults" | grep -E "<paste_key>(\S*)</paste_key>" --context=0 | sed s+\<paste_key\>++g | sed s+'</paste_key>'++g |tr '\r' ' ')

SAVEIFS=$IFS
IFS=$'\n\r'
#shellcheck disable=SC2206
pasteNameArr=($titles)
#shellcheck disable=SC2206
pasteKeyArr=($pasteURLs)
IFS=$SAVEIFS

totalPastes=$((${#pasteNameArr[@]}))



# test if the request response was valid
if [[ $(echo "$queryResults" | grep "paste" -c) -gt 0 || $(echo "$queryResults" | grep "No pastes found" -c) == 1 ]]
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
#print paste title
printf '%s\n' "$((i + 1)): ${pasteNameArr[$i]} |href=https://pastebin.com/${pasteKeyArr[$i]}"

#get and display paste content with slight modifications to prevent breaking Bitbar display
pasteContent="$(curl --silent -X POST --connect-timeout 15 --speed-time 15 --speed-limit 500 -d "api_option=show_paste&api_user_key=$usr_key&api_dev_key=$dev_key&api_paste_key=${pasteKeyArr[$i]}"  $get_paste_url)"

echo "-- $(echo "$pasteContent" | tr '\n' '$' |  tr '\r' '$' | sed 's/\$\$/; /g' | sed 's/;;/; /g')"

#save paste contents temporarily until next refresh

fname=$(echo "${pasteNameArr[$i]}_${pasteKeyArr[$i]}"|tr ' ' '_'|tr '	' '_')
tempname="/tmp/pastebinReader/${fname}.txt"
echo "$pasteContent" > "$tempname"

#display the paste in terminal
echo "---- Read :book: | bash='cat \"$tempname\" |less'"


#if a save directory is given, offer to save the paste
if [[ $saveDir != "" && -d $saveDir ]]
then
echo "---- Save :arrow_down: | bash='cat \"$tempname\" > \"$saveDir/${fname}.txt\"' terminal=false"
else
echo "---- Save Disabled | color=yellow"
echo "------ Go to config section to enable"
fi

#if delete is enabled, give user a choice to delete paste

if [[ $deleteEnabled == 1 ]]
then
echo "---- Delete"
echo "api_option=delete&api_user_key=$usr_key&api_dev_key=$dev_key&api_paste_key=${pasteKeyArr[$i]}" > "/tmp/pastebinReader/${fname}_delete_request.txt"
echo "------ Confirm | color = red bash='curl --silent --connect-timeout 15 --speed-time 15 --speed-limit 500  -X POST --data \"@/tmp/pastebinReader/${fname}_delete_request.txt\" $list_paste_url' terminal=false"
else
echo Delete Disabled
fi

i=$((i + 1))



done

# test if it is a connectivity issue
elif [[ $(curl https://pastebin.com/api/api_post.php --silent --connect-timeout 15 --speed-time 15 --speed-limit 500| grep "Bad API request" -c) == 0  ]]
then

echo ":warning: Disconnected | color=yellow"
echo "---"
echo "Refresh Now | refresh=true color=blue"

else

echo ":warning: Bad API Request | color=red"
echo :warning: Please check keys for errors
echo "---"
echo "Refresh Now | refresh=true color=blue"

fi
