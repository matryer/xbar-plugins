#!/bin/bash

# <bitbar.title>ISS Pass Finder</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Cian Dowd</bitbar.author>
# <bitbar.author.github>BrokenFlows</bitbar.author.github>
# <bitbar.desc>Tells you when the ISS (space station) will pass over your location and for how long it will be above the horizon. Requires user to enter their location data in the script</bitbar.desc>
# <bitbar.image>http://i.imgur.com/z4Tc4dt.png</bitbar.image>
# <bitbar.dependencies>jq</bitbar.dependencies>

echo "Please open the script to enter your location information, you can delete this line once you have" 1>&2
exit 0 #you should also delete this one

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
longi=0                                                                                                                 #
#                                                                                                                       #
# Enter your elevation (you can find it here: www.whatismyelevation.com)                                                #
eleva=0                                                                                                                 #
# ********************************************************************************************************************* #



curl -s --connect-timeout 30 -o /tmp/issn.json "http://api.open-notify.org/iss-now.json"
curl -s --connect-timeout 30 -o /tmp/issp.json "http://api.open-notify.org/iss-pass.json?lat=$latit&lon=$longi&alt=$eleva&n=10"

issnowtu=$(/usr/local/bin/jq -r '.timestamp' /tmp/issn.json)
latit=$(/usr/local/bin/jq -r '.iss_position.latitude' /tmp/issn.json)
longi=$(/usr/local/bin/jq -r '.iss_position.longitude' /tmp/issn.json)
isspd1=$(/usr/local/bin/jq -r '.response[0].duration' /tmp/issp.json)
isspd11=$(printf '%02d:%02d\n' $((isspd1/60%60)) $((isspd1%60)))
isspd2=$(/usr/local/bin/jq -r '.response[1].duration' /tmp/issp.json)
isspd22=$(printf '%02d:%02d\n' $((isspd2/60%60)) $((isspd2%60)))
isspd3=$(/usr/local/bin/jq -r '.response[2].duration' /tmp/issp.json)
isspd33=$(printf '%02d:%02d\n' $((isspd3/60%60)) $((isspd3%60)))
isspd4=$(/usr/local/bin/jq -r '.response[3].duration' /tmp/issp.json)
isspd44=$(printf '%02d:%02d\n' $((isspd4/60%60)) $((isspd4%60)))
isspd5=$(/usr/local/bin/jq -r '.response[4].duration' /tmp/issp.json)
isspd55=$(printf '%02d:%02d\n' $((isspd5/60%60)) $((isspd5%60)))
isspd6=$(/usr/local/bin/jq -r '.response[5].duration' /tmp/issp.json)
isspd66=$(printf '%02d:%02d\n' $((isspd6/60%60)) $((isspd6%60)))
isspd7=$(/usr/local/bin/jq -r '.response[6].duration' /tmp/issp.json)
isspd77=$(printf '%02d:%02d\n' $((isspd7/60%60)) $((isspd7%60)))
isspd8=$(/usr/local/bin/jq -r '.response[7].duration' /tmp/issp.json)
isspd88=$(printf '%02d:%02d\n' $((isspd8/60%60)) $((isspd8%60)))
isspd9=$(/usr/local/bin/jq -r '.response[8].duration' /tmp/issp.json)
isspd99=$(printf '%02d:%02d\n' $((isspd9/60%60)) $((isspd9%60)))
isspd10=$(/usr/local/bin/jq -r '.response[9].duration' /tmp/issp.json)
isspd100=$(printf '%02d:%02d\n' $((isspd10/60%60)) $((isspd10%60)))
isspt1=$(/usr/local/bin/jq -r '.response[0].risetime' /tmp/issp.json)
isspt11=$(date -r "$isspt1" +'%H:%M %d/%m/%y')
isspt2=$(/usr/local/bin/jq -r '.response[1].risetime' /tmp/issp.json)
isspt22=$(date -r "$isspt2" +'%H:%M %d/%m/%y')
isspt3=$(/usr/local/bin/jq -r '.response[2].risetime' /tmp/issp.json)
isspt33=$(date -r "$isspt3" +'%H:%M %d/%m/%y')
isspt4=$(/usr/local/bin/jq -r '.response[3].risetime' /tmp/issp.json)
isspt44=$(date -r "$isspt4" +'%H:%M %d/%m/%y')
isspt5=$(/usr/local/bin/jq -r '.response[4].risetime' /tmp/issp.json)
isspt55=$(date -r "$isspt5" +'%H:%M %d/%m/%y')
isspt6=$(/usr/local/bin/jq -r '.response[5].risetime' /tmp/issp.json)
isspt66=$(date -r "$isspt6" +'%H:%M %d/%m/%y')
isspt7=$(/usr/local/bin/jq -r '.response[6].risetime' /tmp/issp.json)
isspt77=$(date -r "$isspt7" +'%H:%M %d/%m/%y')
isspt8=$(/usr/local/bin/jq -r '.response[7].risetime' /tmp/issp.json)
isspt88=$(date -r "$isspt8" +'%H:%M %d/%m/%y')
isspt9=$(/usr/local/bin/jq -r '.response[8].risetime' /tmp/issp.json)
isspt99=$(date -r "$isspt9" +'%H:%M %d/%m/%y')
isspt10=$(/usr/local/bin/jq -r '.response[9].risetime' /tmp/issp.json)
isspt100=$(date -r "$isspt10" +'%H:%M %d/%m/%y')

curl -s --connect-timeout 30 -o /tmp/gcd.json "https://maps.googleapis.com/maps/api/geocode/json?latlng=$latit,$longi&components=locality&language=en"

issnowl=$(/usr/local/bin/jq -r '.results[0].formatted_address' /tmp/gcd.json)
issnowlc=$(grep "ZERO_RESULTS" /tmp/gcd.json)

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
echo "Visible from $isspt11 for $isspd11 | font=CourierNew"
echo "Visible from $isspt22 for $isspd22 | font=CourierNew"
echo "Visible from $isspt33 for $isspd33 | font=CourierNew"
echo "Visible from $isspt44 for $isspd44 | font=CourierNew"
echo "Visible from $isspt55 for $isspd55 | font=CourierNew"
echo "Visible from $isspt66 for $isspd66 | font=CourierNew"
echo "Visible from $isspt77 for $isspd77 | font=CourierNew"
echo "Visible from $isspt88 for $isspd88 | font=CourierNew"
echo "Visible from $isspt99 for $isspd99 | font=CourierNew"
echo "Visible from $isspt100 for $isspd100 | font=CourierNew"

rm /tmp/issn.json
rm /tmp/issp.json
rm /tmp/gcd.json
exit 0
