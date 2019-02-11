#!/bin/bash
# <bitbar.title>Nightscout Reader</bitbar.title>
# <bitbar.version>0.1.0</bitbar.version>
# <bitbar.desc>For Diabetics using Nightscout to track CGM data: Display current Blood Sugar data and trend from Nightscout</bitbar.desc>
# <bitbar.dependencies>bash, curl, bc</bitbar.dependencies>
# <bitbar.author.github>badgerpapa</bitbar.author.github>
# <bitbar.image>https://raw.githubusercontent.com/badgerpapa/NightscoutBitBar/master/Preview.png</bitbar.image>
# <bitbar.abouturl>https://github.com/badgerpapa/NightscoutBitBar/blob/master/README.md</bitbar.abouturl>

NSURL=http://YOURNIGHTSCOUTURL.herokuapp.com # Add your own Nightscout URL here
USEMMOL=true # true if you use mmol/l units. false if you use mg/dl


read -r -a RESULTARRAY <<< "$(curl --silent $NSURL/api/v1/entries/current)"

TIMESTAMP=${RESULTARRAY[0]//\"}  
TIMESTAMP=${TIMESTAMP//\.[0-9][0-9][0-9]/} #Strip milliseconds from the timestamp. BSD date doesn't like it.
EPOCHTS=$(date -j -f "%FT%T%z" "$TIMESTAMP" +%s) # Convert timestamp to epoch time
EPOCHNOW=$(date +%s) # Convert current time to epoch time
TIMEDIFF=$(((EPOCHNOW - EPOCHTS)/60)) #calculate the difference

BG=${RESULTARRAY[2]}
if $USEMMOL ; then
	BG=$(echo "scale=1; $BG / 18" | bc) #Convert mg/dl to mmol/l
fi


case ${RESULTARRAY[3]//\"} in
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
		TREND=${RESULTARRAY[3]}
		;;
esac

echo "$BG $TREND (${TIMEDIFF}m ago)"
echo "---"
echo "Go to Nightscout | href=$NSURL"
echo "Reading taken: $TIMESTAMP"
