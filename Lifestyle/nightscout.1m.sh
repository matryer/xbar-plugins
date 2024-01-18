#!/bin/bash
# <xbar.title>Nightscout Reader</xbar.title>
# <xbar.version>0.2.0</xbar.version>
# <xbar.desc>For Diabetics using Nightscout to track CGM data: Display current Blood Sugar data and trend from Nightscout</xbar.desc>
# <xbar.dependencies>bash, curl, bc, jq</xbar.dependencies>
# <xbar.author>Jeremy Hay Draude</xbar.author>
# <xbar.author.github>jhaydraude</xbar.author.github>
# <xbar.image>https://raw.githubusercontent.com/jhaydraude/NightscoutBitBar/master/Preview.png</xbar.image>
# <xbar.abouturl>https://github.com/jhaydraude/NightscoutBitBar/blob/master/README.md</xbar.abouturl>
#
# <xbar.var>string(VAR_NSURL=""): Your own Nightscout URL.</xbar.var>
# <xbar.var>string(VAR_TOKEN=""): Your own Nightscout API Token</xbar.var>
# <xbar.var>boolean(VAR_USEMMOL=true): Use mmol/l otherweise mg/dl</xbar.var>

export PATH="/usr/local/bin:/usr/bin:$PATH"

if ! [ -x "$(command -v jq)" ]; then
  echo 'Error: jq is not installed.'
  echo '---'
  echo 'Click here to install jq | href=https://stedolan.github.io/jq/download/'
  exit 1
fi

if [ -n "$VAR_TOKEN" ]; then
    JSONOUT=$(curl --silent -H "API-SECRET:${VAR_TOKEN}" "${VAR_NSURL}/api/v1/entries/current.json")
else
    JSONOUT=$(curl --silent "${VAR_NSURL}/api/v1/entries/current.json")
fi

JSDATE=$(jq '.[0].date' <<< "$JSONOUT")
#NS Returns date as ms since epoch. BASH likes Seconds since epoch
EPOCHTS=$((JSDATE / 1000))
TIMESTRING=$(date -r $EPOCHTS)

EPOCHNOW=$(date +%s) # Convert current time to epoch time
TIMEDIFF=$(((EPOCHNOW - EPOCHTS)/60)) #calculate the difference

BG=$(jq '.[0].sgv' <<< "$JSONOUT")

TRENDSTR=$(jq -r '.[0].direction' <<< "$JSONOUT")

if $VAR_USEMMOL ; then
	BG=$(echo "scale=1; $BG / 18" | bc) #Convert mg/dl to mmol/l
fi


case $TRENDSTR in
	FortyFiveUp)
		TREND='↗'
		;;
	FortyFiveDown)
		TREND='↘'
		;;
	SingleUp)
		TREND='↑'
		;;
	SingleDown)
		TREND='↓'
		;;
	Flat)
		TREND='→'
		;;
	DoubleUp)
		TREND='⇈'
		;;
	DoubleDown)
		TREND='⇊'
		;;
	*)
		TREND=$TRENDSTR
		;;
esac

echo "$BG $TREND (${TIMEDIFF}m ago)"
echo "---"
echo "Go to Nightscout | href=${VAR_NSURL}"
echo "Reading taken: $TIMESTRING"
