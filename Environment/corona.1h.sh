#!/bin/bash

# <bitbar.title>Corona info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Joakim Ramer</bitbar.author>
# <bitbar.author.github>jramer</bitbar.author.github>
# <bitbar.desc>Shows corona virus data from https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6 and alerts you when the corona virus comes to your country.</bitbar.desc>
# <bitbar.dependencies>bash, curl, jq, fping</bitbar.dependencies>
# <bitbar.image>https://github.com/jramer/bitbar-corona/raw/master/corona_info.png</bitbar.image>
# <bitbar.abouturl>https://github.com/jramer/bitbar-corona</bitbar.abouturl>

COUNTRY='Sweden'
COUNTRY_FLAG='🇸🇪'
WARNING_THRESHOLD=0

FPING_PATH=/usr/local/bin/fping
CURL_PATH=/usr/bin/curl
JQ_PATH=/usr/local/bin/jq

API_URL='https://services1.arcgis.com/0MSEUqKaxRlEPj5g/arcgis/rest/services/ncov_cases/FeatureServer/2/query'
PARAMS='f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=Confirmed%20desc&resultOffset=0&resultRecordCount=250&cacheHint=true'

# Check internet
COUNT=6
until (( COUNT == 0 )); do
  $FPING_PATH -c1 -t300 8.8.8.8 2>/dev/null 1>/dev/null
  if (( $? == 0 )); then
    break
  fi

  if (( --COUNT == 0 )); then
    echo "-"
    exit
  fi

  sleep 10
done

RESPONSE=$($CURL_PATH -s -X GET $API_URL?$PARAMS -H 'Accept: application/json')
CONFIRMED=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes.Confirmed] | reduce .[] as \$num (0; .+\$num)")
DEATHS=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes.Deaths] | reduce .[] as \$num (0; .+\$num)")
RECOVERED=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes.Recovered] | reduce .[] as \$num (0; .+\$num)")
COUNTRIES_INFECTED=$(echo $RESPONSE | $JQ_PATH ".features | length")

IN_COUNTRY=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes.Country_Region] | map(select(. == \"$COUNTRY\")) | if . == [] then \"false\" else \"true\" end")

if [ "$IN_COUNTRY" = '"true"' ]; then
   COUNTRY_CONFIRMED=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes] | map(select(.Country_Region == \"$COUNTRY\")) | .[].Confirmed")
   COUNTRY_DEATHS=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes] | map(select(.Country_Region == \"$COUNTRY\")) | .[].Deaths")
   COUNTRY_RECOVERED=$(echo $RESPONSE | $JQ_PATH "[.features[].attributes] | map(select(.Country_Region == \"$COUNTRY\")) | .[].Recovered")

   if [ "$COUNTRY_CONFIRMED" -gt "$WARNING_THRESHOLD" ]; then
      echo "$COUNTRY_FLAG 🔴 😷"
   else
      echo "$COUNTRY_FLAG 🟢"
  fi
   echo "---"
   echo "$COUNTRY: | color=white"
   echo "😷 $COUNTRY_CONFIRMED"
   echo "☠️ $COUNTRY_DEATHS | color=red"
   echo "😇 $COUNTRY_RECOVERED | color=green"
else
   echo "$COUNTRY_FLAG 🟢"
fi

echo "---"
echo "The World: | color=white"
echo "😷 $CONFIRMED"
echo "☠️ $DEATHS | color=red"
echo "😇 $RECOVERED | color=green"
echo "🗺 $COUNTRIES_INFECTED"
echo "---"
echo "↻ - Refresh| terminal=false refresh=true"
echo "🌍 Check data origin | href=https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6"
