#!/bin/bash

#
# <xbar.title>Check Pastebin</xbar.title>
# <xbar.version>v1.2</xbar.version>
# <xbar.author>Tyllis Xu</xbar.author>
# <xbar.author.github>livelycarpet87</xbar.author.github>
# <xbar.desc>Uses a set of Pastebin API keys to check for pastes created by the user. It will provide links to all the pastes it finds by the user. </xbar.desc>
# <xbar.image>https://i.ibb.co/cym797V/Pastebin2.png</xbar.image>
# <xbar.abouturl>https://github.com/LivelyCarpet87/BitBar-Pastebin#bitbar-pastebin</xbar.abouturl>
# <xbar.var>string(VAR_VAR_DEV_KEY=""): Your Pastebin API developer key</xbar.var>
# <xbar.var>string(VAR_VAR_USR_KEY=""): Your Pastebin API user key</xbar.var>
# <xbar.var>string(VAR_VAR_SAVE_DIRECTORY=""): The directory to download pastes to</xbar.var>
# <xbar.var>string(VAR_VAR_DELETE_ENABLED=1): Whether if the plugin should be allowed to delete pastes (per user request) [0,1]</xbar.var>
#

if [[ $VAR_DEV_KEY == '' ]]
then
  echo ":warning: Dev key not set | color=#DAA520"
  echo "---"
  echo "Refresh Now | refresh=true color=blue"
  exit
fi
if [[ $VAR_USR_KEY == '' ]]
then
  echo ":warning: Dev key not set | color=#DAA520"
  echo "---"
  echo "Refresh Now | refresh=true color=blue"
  exit
fi

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

queryResults=$(curl --silent --connect-timeout 15 --speed-time 15 --speed-limit 500  -X POST -d "api_option=list&api_user_key=$VAR_USR_KEY&api_dev_key=$VAR_DEV_KEY" $list_paste_url)

#parse the paste titles and keys

titles=$(echo "$queryResults" | grep -E "<paste_title>([^\r]*)" --context=0 | sed s+'<paste_title></paste_title>'+Untitled+g | sed s+\<paste_title\>++g | sed s+'</paste_title>'++g |tr '\r' ' ')

pasteURLs=$(echo "$queryResults" | grep -E "<paste_key>(\S*)</paste_key>" --context=0 | sed s+\<paste_key\>++g | sed s+'</paste_key>'++g |tr '\r' ' ' | sed s+[[:space:]]++g)

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
printf '%s\n' "$((i + 1)): ${pasteNameArr[$i]} | href='https://pastebin.com/${pasteKeyArr[$i]}'"

#get and display paste content with slight modifications to prevent breaking Bitbar display
pasteContent="$(curl --silent -X POST --connect-timeout 15 --speed-time 15 --speed-limit 500 -d "api_option=show_paste&api_user_key=$VAR_USR_KEY&api_dev_key=$VAR_DEV_KEY&api_paste_key=${pasteKeyArr[$i]}"  $get_paste_url)"

echo "-- $(echo "$pasteContent" | tr '\n' '$' |  tr '\r' '$' | sed 's/\$\$/; /g' | sed 's/;;/; /g' | sed s+\|\|+OR+g | sed s+\|+?+g)"

#save paste contents temporarily until next refresh

fname=$(echo "${pasteNameArr[$i]}_${pasteKeyArr[$i]}"|tr ' ' '_'| tr '#' 'H'|tr '$' 'S'| tr '&' '+'| tr -d '>'| tr -d '<'| tr -d '[' | tr -d ']'| tr -d '(' | tr -d ')' | tr -d '`' | tr -d '"' | tr -d \'| tr -d \\ | tr -d '|' | tr -d ';' |tr -d '*' | tr -d '?'| tr -d '~' | tr -d '/')
tempname="/tmp/pastebinReader/${fname}.txt"
echo "$pasteContent" > "$tempname"

#display the paste in terminal
echo "---- Read :book: | shell=open param1='${tempname}' terminal=false"


#if a save directory is given, offer to save the paste
if [[ $VAR_SAVE_DIRECTORY != "" && -d "$VAR_SAVE_DIRECTORY" ]]
then
echo "---- Save :arrow_down: | shell='cp' param1='$tempname' param2='$VAR_SAVE_DIRECTORY/${fname}.txt' terminal=false"
else
echo "---- Save Disabled | color=#DAA520"
echo "------ Go to config section to add a valid save path"
fi
#if delete is enabled, give user a choice to delete paste
if [[ $VAR_DELETE_ENABLED == 1 ]]
then
echo "---- Delete"
echo "api_option=delete&api_user_key=$VAR_USR_KEY&api_dev_key=$VAR_DEV_KEY&api_paste_key=${pasteKeyArr[$i]}" > "/tmp/pastebinReader/${fname}_delete_request.txt"
echo "------ Confirm | color=#800000 shell='curl' param1='-d' param2='@/tmp/pastebinReader/${fname}_delete_request.txt' param3='$list_paste_url' refresh=true terminal=false"
else
echo "---- Delete Disabled | color=#DAA520"
echo "------ Go to config section to enable"
fi

i=$((i + 1))



done

# test if it is a connectivity issue
elif [[ $(curl https://pastebin.com/api/api_post.php --silent --connect-timeout 15 --speed-time 15 --speed-limit 500| grep "Bad API request" -c) == 0  ]]
then

echo ":warning: Disconnected | color=#DAA520"
echo "---"
echo "Refresh Now | refresh=true color=blue"

else

echo ":warning: Bad API Request | color=#800000"
echo ":warning: Please check keys for errors"
echo "---"
echo "Refresh Now | refresh=true color=blue"

fi
