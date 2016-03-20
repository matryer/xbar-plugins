#!/bin/bash
# <bitbar.title>CPU Temperature</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Eric Ripa</bitbar.author>
# <bitbar.author.github>eripa</bitbar.author.github>
# <bitbar.desc>This plugin displays the current CPU temperature (requires external 'smc' binary)</bitbar.desc>
# <bitbar.dependencies>smc</bitbar.dependencies>
#
# 'smc' can be downloaded from: http://www.eidac.de/smcfancontrol/smcfancontrol_2_4.zip
# One-liner:
# curl -LO http://www.eidac.de/smcfancontrol/smcfancontrol_2_4.zip && unzip -d temp_dir_smc smcfancontrol_2_4.zip && cp temp_dir_smc/smcFanControl.app/Contents/Resources/smc /usr/local/bin/smc ; rm -rf temp_dir_smc smcfancontrol_2_4.zip

FAHRENHEIT=false
TEMPERATURE_WARNING_LIMIT=80
TEMPERATURE=$(/usr/local/bin/smc -k TC0P -r | sed 's/.*bytes \(.*\))/\1/' |sed 's/\([0-9a-fA-F]*\)/0x\1/g' | perl -ne 'chomp; ($low,$high) = split(/ /); print (((hex($low)*256)+hex($high))/4/64); print "\n";')
TEMP_INTEGER=${TEMPERATURE%.*}

if $FAHRENHEIT ; then
  TEMP_INTEGER=$((TEMP_INTEGER*9/5+32))
  LABEL="°f"
else
  LABEL="°c"
fi

if [ "$TEMP_INTEGER" -gt "$TEMPERATURE_WARNING_LIMIT" ] ; then
  ICON="🔥"
else
  ICON=""
fi
echo "$ICON${TEMP_INTEGER}$LABEL| size=12"
