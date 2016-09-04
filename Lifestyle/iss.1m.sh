#!/bin/bash

# <bitbar.title>ISS Pass Finder</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cian Dowd</bitbar.author>
# <bitbar.author.github>BrokenFlows</bitbar.author.github>
# <bitbar.desc>Tells you when the ISS (space station) will pass over your location and for how long it will be above the horizon. Requires user to enter their location data in the script</bitbar.desc>
# <bitbar.image>https://puu.sh/qZuIP/77aff0bf10.png</bitbar.image>
# <bitbar.dependencies>jq</bitbar.dependencies>



echo "Please open the script to enter your location information, you can delete this line once you have"
echo "---" #you should also delete this one

# Credit: I am not the original author of this script, I altered an existing Linux script for BSD Unix and BitBar. The original Shell Script can be found here: http://vsido.org/index.php?topic=1016.0

# Note: Even if the International Space Station is above the horizon if may only be visible in the late evening and and night, as is the case with stars. Happy spotting

# User set up in the box below
# ********************************************************************************************************************* #
# This script requires jq, which you can get by typing                                                                  #
#                   brew install jq                                                                                     #
# into the terminal app if you have homebrew installed                                                                  #
#                                                                                                                       #
# Please leave out any units                                                                                            #
# Enter your latitude and longitude (you can find them here: http://mynasadata.larc.nasa.gov/latitudelongitude-finder/) #
latit=0                                                                                                                 #
longi=0                                                                                                                  #
#                                                                                                                       #
# Enter your elevation (you can find it here: www.whatismyelevation.com)                                                #
eleva=0                                                                                                                  #
# ********************************************************************************************************************* #




curl -s --connect-timeout 30 -o /tmp/issn.json "http://api.open-notify.org/iss-now.json"
curl -s --connect-timeout 30 -o /tmp/issp.json "http://api.open-notify.org/iss-pass.json?lat=$latit&lon=$longi&alt=$eleva&n=10"
curl -s --connect-timeout 30 -o /tmp/astros.json "http://api.open-notify.org/astros.json"

issnowtu=$(cat /tmp/issn.json | /usr/local/bin/jq -r '.timestamp')
latit=$(cat /tmp/issn.json | /usr/local/bin/jq -r '.iss_position.latitude')
longi=$(cat /tmp/issn.json | /usr/local/bin/jq -r '.iss_position.longitude')
isspd1=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[0].duration')
isspd11=$(printf '%02d:%02d\n' $((isspd1/60%60)) $((isspd1%60)))
isspd2=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[1].duration')
isspd22=$(printf '%02d:%02d\n' $((isspd2/60%60)) $((isspd2%60)))
isspd3=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[2].duration')
isspd33=$(printf '%02d:%02d\n' $((isspd3/60%60)) $((isspd3%60)))
isspd4=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[3].duration')
isspd44=$(printf '%02d:%02d\n' $((isspd4/60%60)) $((isspd4%60)))
isspd5=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[4].duration')
isspd55=$(printf '%02d:%02d\n' $((isspd5/60%60)) $((isspd5%60)))
isspd6=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[5].duration')
isspd66=$(printf '%02d:%02d\n' $((isspd6/60%60)) $((isspd6%60)))
isspd7=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[6].duration')
isspd77=$(printf '%02d:%02d\n' $((isspd7/60%60)) $((isspd7%60)))
isspd8=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[7].duration')
isspd88=$(printf '%02d:%02d\n' $((isspd8/60%60)) $((isspd8%60)))
isspd9=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[8].duration')
isspd99=$(printf '%02d:%02d\n' $((isspd9/60%60)) $((isspd9%60)))
isspd10=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[9].duration')
isspd100=$(printf '%02d:%02d\n' $((isspd10/60%60)) $((isspd10%60)))
isspt1=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[0].risetime')
isspt11=$(date -r "$isspt1" +'%H:%M %d/%m/%y')
isspt2=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[1].risetime')
isspt22=$(date -r "$isspt2" +'%H:%M %d/%m/%y')
isspt3=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[2].risetime')
isspt33=$(date -r "$isspt3" +'%H:%M %d/%m/%y')
isspt4=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[3].risetime')
isspt44=$(date -r "$isspt4" +'%H:%M %d/%m/%y')
isspt5=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[4].risetime')
isspt55=$(date -r "$isspt5" +'%H:%M %d/%m/%y')
isspt6=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[5].risetime')
isspt66=$(date -r "$isspt6" +'%H:%M %d/%m/%y')
isspt7=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[6].risetime')
isspt77=$(date -r "$isspt7" +'%H:%M %d/%m/%y')
isspt8=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[7].risetime')
isspt88=$(date -r "$isspt8" +'%H:%M %d/%m/%y')
isspt9=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[8].risetime')
isspt99=$(date -r "$isspt9" +'%H:%M %d/%m/%y')
isspt10=$(cat /tmp/issp.json | /usr/local/bin/jq -r '.response[9].risetime')
isspt100=$(date -r "$isspt10" +'%H:%M %d/%m/%y')
astrock=$(cat /tmp/astros.json | grep "0,")

curl -s --connect-timeout 30 -o /tmp/gcd.json "https://maps.googleapis.com/maps/api/geocode/json?latlng=$latit,$longi&components=locality&language=en"

issnowl=$(cat /tmp/gcd.json | /usr/local/bin/jq -r '.results[0].formatted_address')
issnowlc=$(cat /tmp/gcd.json | grep "ZERO_RESULTS")

issptl=$((isspt1 - issnowtu))

echo "ISS passing in $((issptl/3600%60)) hours $((issptl/60%60)) minutes"
echo "---"

if [[ -z "$issnowlc" ]]; then
		echo -e "The ISS is over $issnowl"
else
		echo -e "The ISS is over an unmapped area"
fi

echo "---"
echo ""
echo -e "ISS will pass over your location at these times:"
echo "Visible from $isspt11 for $isspd11"
echo "Visible from $isspt22 for $isspd22"
echo "Visible from $isspt33 for $isspd33"
echo "Visible from $isspt44 for $isspd44"
echo "Visible from $isspt55 for $isspd55"
echo "Visible from $isspt66 for $isspd66"
echo "Visible from $isspt77 for $isspd77"
echo "Visible from $isspt88 for $isspd88"
echo "Visible from $isspt99 for $isspd99"
echo "Visible from $isspt100 for $isspd100"

rm /tmp/issn.json
rm /tmp/issp.json
rm /tmp/astros.json
rm /tmp/gcd.json
exit 0

