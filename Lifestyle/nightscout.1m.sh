#!/bin/bash
# <xbar.title>Nightscout Reader</xbar.title>
# <xbar.version>0.2.0</xbar.version>
# <xbar.desc>For Diabetics using Nightscout to track CGM data: Display current Blood Sugar data and trend from Nightscout</xbar.desc>
# <xbar.dependencies>bash, curl, bc, jq</xbar.dependencies>
# <xbar.author>Jeremy Hay Draude</xbar.author>
# <xbar.author.github>jhaydraude</xbar.author.github>
# <xbar.image>https://raw.githubusercontent.com/jhaydraude/NightscoutBitBar/master/Preview.png</xbar.image>
# <xbar.abouturl>https://github.com/jhaydraude/NightscoutBitBar/blob/master/README.md</xbar.abouturl>

export PATH="/usr/local/bin:/usr/bin:$PATH"

if ! [ -x "$(command -v jq)" ]; then
  echo 'Error: jq is not installed.'
  echo '---'
  echo 'Click here to install jq | href=https://stedolan.github.io/jq/download/'
  exit 1
fi

NSURL=https://db40.med-data.online/ # Add your own Nightscout URL here
USEMMOL=true # true if you use mmol/l units. false if you use mg/dl

JSONOUT=$(curl --silent $NSURL/api/v1/entries/current.json)

JSDATE=$(jq '.[0].date' <<< "$JSONOUT")
#NS Returns date as ms since epoch. BASH likes Seconds since epoch
EPOCHTS=$((JSDATE / 1000))
TIMESTRING=$(date -r $EPOCHTS)

EPOCHNOW=$(date +%s) # Convert current time to epoch time
TIMEDIFF=$(((EPOCHNOW - EPOCHTS)/60)) #calculate the difference

BG=$(jq '.[0].sgv' <<< "$JSONOUT")

TRENDSTR=$(jq -r '.[0].direction' <<< "$JSONOUT")

if $USEMMOL ; then
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
echo "Go to Nightscout | href=$NSURL"
echo "Reading taken: $TIMESTRING"
