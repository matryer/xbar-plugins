#!/bin/bash
# <bitbar.title>Fan Control Info</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Derman Enterprises, 2016</bitbar.author>
# <bitbar.desc>This plugin displays the current temperature and fan-speed info from Fan Control</bitbar.desc>
# <bitbar.image>https://www.derman.com/Resources/Misc/FanControl/BitBar1fanC.png</bitbar.image>
# <bitbar.dependencies>Fan Control</bitbar.dependencies>
# <bitbar.abouturl>https://www.derman.com/Fan-Control</bitbar.abouturl>
#
# 'Fan Control' can be downloaded from: https://www.derman.com/Fan-Control

#set -x  ## uncomment for a trace

# convert the Celcius temperature, given as the first argument, to Fahrenheit
function CtoF()
{
	if test $# -eq 2
	then
		DECS=$2
	else
		DECS=0
	fi

	F_TEMP=`echo "scale=4; ((${1} * 9) / 5) + 32" | /usr/bin/bc`
	LC_ALL=C /usr/bin/printf "%.*f\n" $DECS $F_TEMP
}

# set the inter-field separator to be only a newline
NL='
'
IFS="$NL"

# define the threshold temps (in celcius) after which menu-display color changes
C_TEMP_NORM_LIMIT=40
C_TEMP_WARNING_LIMIT=60
C_TEMP_DANGER_LIMIT=77
F_TEMP_NORM_LIMIT=105
F_TEMP_WARNING_LIMIT=140
F_TEMP_DANGER_LIMIT=170
TEMP_NORM_LIMIT=''
TEMP_WARNING_LIMIT=''
TEMP_DANGER_LIMIT=''

BIT_BAR_PREFS_FILE=~/Library/Preferences/com.derman.FanControl.BitBar.txt
PREFS_FILE='/Library/Preferences/com.derman.FanControl.plist'
FC_UTIL='/Library/PreferencePanes/FanControl.prefPane/Contents/Resources/getFanControlInfo'
BITBAR_PREF_SETTER='/Library/PreferencePanes/FanControl.prefPane/Contents/Resources/setBitBarPref.sh'

# font/output colors
BLK='color=#505050'
GRN='color=#005500'
GRY='color=#555555'
LT_GRY='color=#999999'
ORG='color=#C04C32'
RED='color=#AA0000'
FONT='size=12 font=Tahoma-Bold'
#FONT='size=13 font=HelveticaNeue-Bold'  # this one also works well in menu bar

# a temperature of -100.0 & a fan speed of -1 indicates no reading is available
BIT_BAR_PREFS=''
BITBAR_SHOW_AS_FAHRENHEIT=''
COLOR="$GRN"
DISPLAY_TEMP=''
FAN_ICON="♨ "
FAN1_MAX_RPM=''
FAN2_MAX_RPM=''
FAN3_MAX_RPM=''
FAN1_MIN_RPM=''
FAN2_MIN_RPM=''
FAN3_MIN_RPM=''
FAN1_NAME=''
FAN2_NAME=''
FAN3_NAME=''
FAN1_PCT=''
FAN2_PCT=''
FAN3_PCT=''
FAN1_SPEED='-1'
FAN2_SPEED='-1'
FAN3_SPEED='-1'
FAN_DIVIDER='/'
FAN_PCT_SUFFIX=''
FAN_SPEEDS=''
HIGHEST_CPU_TEMP='-100.0'
HIGHEST_GPU_TEMP='-100.0'
HIGHEST_FULL_TEMP='-100.0'
HIGHEST_FULL_CPU_TEMP='-100.0'
HIGHEST_FULL_GPU_TEMP='-100.0'
HIGHEST_TEMP='-100.0'
HIGHEST_TEMP_NAME=''
HIGH_TEMP_ICON=''
LOWER_TEMP_THRESHOLD=''
NUM_FANS='0'
PREFS=''
SHOW_AS_FAHRENHEIT='false'
SHOW_AS_FAHRENHEIT_FC='false'
SHOW_CPU_TEMP='false'
SHOW_FAN_PCTS='false'
SHOW_FANS='true'
SHOW_FAN_NAMES='false'
SHOW_FAN_SPEEDS='true'
SHOW_GPU_TEMP='false'
SHOW_TEMPS='true'
SHOW_TEMP_NAMES='true'
SMC_FAN_INFO=''
SMC_TEMP_ENTRIES=''
TEMP_DIVIDER='/'
TEMP_FAN_DIVIDER='➨'
TEMPS=''
TEMP_LABEL=" °c"
TEMP_LABEL_MBAR="°c"
UPPER_TEMP_THRESHOLD=''

# ensure the Fan Control utility executable file is available
if test ! -x "$FC_UTIL"
then
	echo "${FAN_ICON}Missing Fan Control utility ...| size=12 $RED"
	echo "---"
	echo "ERROR: no 'getFanControlInfo' utility exists ($FC_UTIL)| $RED"
	echo "(it's installed as part of the Fan Control preference pane)"
	exit 1
fi

# get the information from the Fan Control daemon
FC_INFO=`"$FC_UTIL"`

if test "$FC_INFO" = ''
then
	echo "${FAN_ICON}No information from the Fan Control utility ...| size=12 $RED"
	echo "---"
	echo "ERROR: no information was returned from the 'getFanControlInfo' utility ($FC_UTIL)| $RED"
	echo "       (this may indicate that the Fan Control daemon is not running)"
	exit 1
fi

# get the BitBar preference settings
if test -e "$BIT_BAR_PREFS_FILE"
then
	BIT_BAR_PREFS=`/bin/cat "$BIT_BAR_PREFS_FILE"`

	# get the BitBar-only pref for the temperature-scale setting
	BITBAR_SHOW_AS_FAHRENHEIT=`/bin/cat "$BIT_BAR_PREFS_FILE" | \
											/usr/bin/grep '^showTempsAsDegFinBitBar=' | \
											/usr/bin/sed -e 's/showTempsAsDegFinBitBar=//'`

	if test "$BITBAR_SHOW_AS_FAHRENHEIT" != 'false' -a \
			  "$BITBAR_SHOW_AS_FAHRENHEIT" != 'true'
	then
		BITBAR_SHOW_AS_FAHRENHEIT=''
	fi

	# get the BitBar preferences for the items to be shown in the menu-bar
	SHOW_TEMPS=`echo "$BIT_BAR_PREFS" | /usr/bin/grep '^showTempsInMenuBar=' | \
												/usr/bin/sed -e 's/showTempsInMenuBar=//'`

	if test "$SHOW_TEMPS" != 'false'
	then
		SHOW_TEMPS='true'
		SHOW_CPU_TEMP=`echo "$BIT_BAR_PREFS" | \
												/usr/bin/grep '^showCPUtempInMenuBar=' | \
												/usr/bin/sed -e 's/showCPUtempInMenuBar=//'`

		if test "$SHOW_CPU_TEMP" != 'true'
		then
			SHOW_CPU_TEMP='false'
		fi

		SHOW_GPU_TEMP=`echo "$BIT_BAR_PREFS" | \
												/usr/bin/grep '^showGPUtempInMenuBar=' | \
												/usr/bin/sed -e 's/showGPUtempInMenuBar=//'`

		if test "$SHOW_GPU_TEMP" != 'true'
		then
			SHOW_GPU_TEMP='false'
		fi

		SHOW_TEMP_NAMES=`echo "$BIT_BAR_PREFS" | \
											/usr/bin/grep '^showTempNamesInMenuBar=' | \
											/usr/bin/sed -e 's/showTempNamesInMenuBar=//'`

		if test "$SHOW_TEMP_NAMES" != 'false'
		then
			SHOW_TEMP_NAMES='true'
		fi
	fi

	SHOW_FANS=`echo "$BIT_BAR_PREFS" | \
												/usr/bin/grep '^showFansInMenuBar=' | \
												/usr/bin/sed -e 's/showFansInMenuBar=//'`

	if test "$SHOW_FANS" != 'false'
	then
		SHOW_FANS='true'
		SHOW_FAN_NAMES=`echo "$BIT_BAR_PREFS" | \
											/usr/bin/grep '^showFanNamesInMenuBar=' | \
											/usr/bin/sed -e 's/showFanNamesInMenuBar=//'`

		if test "$SHOW_FAN_NAMES" != 'true'
		then
			SHOW_FAN_NAMES='false'
		fi

		SHOW_FAN_SPEEDS=`echo "$BIT_BAR_PREFS" | \
											/usr/bin/grep '^showFanSpeedsInMenuBar=' | \
											/usr/bin/sed -e 's/showFanSpeedsInMenuBar=//'`

		if test "$SHOW_FAN_SPEEDS" != 'false'
		then
			SHOW_FAN_SPEEDS='true'
			FAN_PCT_SUFFIX='@'  # the suffix to use when showing fan percentages
		fi

		SHOW_FAN_PCTS=`echo "$BIT_BAR_PREFS" | \
										/usr/bin/grep '^showFanPercentsInMenuBar=' | \
										/usr/bin/sed -e 's/showFanPercentsInMenuBar=//'`

		if test "$SHOW_FAN_PCTS" != 'true'
		then
			SHOW_FAN_PCTS='false'
		fi
	fi

	if test "$SHOW_FAN_SPEEDS" = 'false' -a "$SHOW_FAN_PCTS" = 'false'
	then
		SHOW_FAN_SPEEDS='true'
	fi
fi

# get the temmperature-scale setting from the Fan Control daemon
SHOW_AS_FAHRENHEIT_FC=`\
						echo "$FC_INFO" | /usr/bin/fgrep 'showTempsAsFahrenheit=' | \
											/usr/bin/sed -e 's/^showTempsAsFahrenheit=//'`
if test "$SHOW_AS_FAHRENHEIT_FC" = ''; then SHOW_AS_FAHRENHEIT_FC='false'; fi

# if there's no BitBar-only temp-scale preference, use the Fan Control pref
if test "$BITBAR_SHOW_AS_FAHRENHEIT" = ''
then
	if test "$SHOW_AS_FAHRENHEIT_FC" = 'true'
	then
		TEMP_LABEL=" °f"
		TEMP_LABEL_MBAR="°f"
		SHOW_AS_FAHRENHEIT='true'
	else
		SHOW_AS_FAHRENHEIT='false'
	fi
else
	if test "$BITBAR_SHOW_AS_FAHRENHEIT" = 'true'
	then
		TEMP_LABEL=" °f"
		TEMP_LABEL_MBAR="°f"
		SHOW_AS_FAHRENHEIT='true'
	else
		SHOW_AS_FAHRENHEIT='false'
	fi
fi

# determine lower temp threshold (before which there's no fan-speed adjustment)
LOWER_TEMP_THRESHOLD=`echo "$FC_INFO" | /usr/bin/fgrep 'lowerTempThreshold=' | \
												/usr/bin/sed -e 's/^lowerTempThreshold=//'`
if test "$LOWER_TEMP_THRESHOLD" = ''; then LOWER_TEMP_THRESHOLD='-100'; fi

# determine the upper temp threshold (after which the fan is to be at max RPM)
UPPER_TEMP_THRESHOLD=`echo "$FC_INFO" | /usr/bin/fgrep 'upperTempThreshold=' | \
												/usr/bin/sed -e 's/^upperTempThreshold=//'`
if test "$UPPER_TEMP_THRESHOLD" = ''; then UPPER_TEMP_THRESHOLD='-100'; fi

# get the highest CPU/GPU-related temperatures
HIGHEST_FULL_CPU_TEMP=`echo "$FC_INFO" | /usr/bin/fgrep 'cpuTemp=' | \
																/usr/bin/sed -e 's/^cpuTemp=//'`
if test "$HIGHEST_FULL_CPU_TEMP" = ''; then HIGHEST_FULL_CPU_TEMP='-100'; fi

HIGHEST_FULL_GPU_TEMP=`echo "$FC_INFO" | /usr/bin/fgrep 'gpuTemp=' | \
																/usr/bin/sed -e 's/^gpuTemp=//'`
if test "$HIGHEST_FULL_GPU_TEMP" = ''; then HIGHEST_FULL_GPU_TEMP='-100'; fi

# determine the number of fans
NUM_FANS=`echo "$FC_INFO" | /usr/bin/fgrep 'numFans=' | \
																/usr/bin/sed -e 's/^numFans=//'`
if test "$NUM_FANS" = ''; then NUM_FANS='0'; fi

# determine the name, min/max/% fan speed and current fan speed for each fan
if test $NUM_FANS -gt 0
then
	FAN1_NAME=`\
		echo "$FC_INFO" | /usr/bin/fgrep 'fan1name=' | \
															/usr/bin/sed -e 's/^fan1name=//'`

	FAN1_MIN_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'minFan1speed=' | \
														/usr/bin/sed -e 's/^minFan1speed=//'`
	if test "$FAN1_MIN_RPM" = ''; then FAN1_MIN_RPM='-1'; fi

	FAN1_MAX_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'maxFan1speed=' | \
														/usr/bin/sed -e 's/^maxFan1speed=//'`
	if test "$FAN1_MAX_RPM" = ''; then FAN1_MAX_RPM='-1'; fi

	FAN1_SPEED=`echo "$FC_INFO" | /usr/bin/fgrep 'currFan1speed=' | \
														/usr/bin/sed -e 's/^currFan1speed=//'`
	if test "$FAN1_SPEED" = ''; then FAN1_SPEED='-1'; fi

	FAN1_PCT=`echo "scale=4; ($FAN1_SPEED / $FAN1_MAX_RPM) * 100" | /usr/bin/bc`
	FAN1_PCT=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $FAN1_PCT`
fi

if test $NUM_FANS -gt 1
then
	FAN2_NAME=`\
		echo "$FC_INFO" | /usr/bin/fgrep 'fan2name=' | \
															/usr/bin/sed -e 's/^fan2name=//'`

	FAN2_MIN_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'minFan2speed=' | \
														/usr/bin/sed -e 's/^minFan2speed=//'`
	if test "$FAN2_MIN_RPM" = ''; then FAN2_MIN_RPM='-1'; fi

	FAN2_MAX_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'maxFan2speed=' | \
														/usr/bin/sed -e 's/^maxFan2speed=//'`
	if test "$FAN2_MAX_RPM" = ''; then FAN2_MAX_RPM='-1'; fi

	FAN2_SPEED=`echo "$FC_INFO" | /usr/bin/fgrep 'currFan2speed=' | \
														/usr/bin/sed -e 's/^currFan2speed=//'`
	if test "$FAN2_SPEED" = ''; then FAN2_SPEED='-1'; fi

	FAN2_PCT=`echo "scale=4; ($FAN2_SPEED / $FAN2_MAX_RPM) * 100" | /usr/bin/bc`
	FAN2_PCT=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $FAN2_PCT`
fi

if test $NUM_FANS -gt 2
then
	FAN3_NAME=`\
		echo "$FC_INFO" | /usr/bin/fgrep 'fan3name=' | \
															/usr/bin/sed -e 's/^fan3name=//'`

	FAN3_MIN_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'minFan3speed=' | \
														/usr/bin/sed -e 's/^minFan3speed=//'`
	if test "$FAN3_MIN_RPM" = ''; then FAN3_MIN_RPM='-1'; fi

	FAN3_MAX_RPM=`echo "$FC_INFO" | /usr/bin/fgrep 'maxFan3speed=' | \
														/usr/bin/sed -e 's/^maxFan3speed=//'`
	if test "$FAN3_MAX_RPM" = ''; then FAN3_MAX_RPM='-1'; fi

	FAN3_SPEED=`echo "$FC_INFO" | /usr/bin/fgrep 'currFan3speed=' | \
														/usr/bin/sed -e 's/^currFan3speed=//'`
	if test "$FAN3_SPEED" = ''; then FAN3_SPEED='-1'; fi

	FAN3_PCT=`echo "scale=4; ($FAN3_SPEED / $FAN3_MAX_RPM) * 100" | /usr/bin/bc`
	FAN3_PCT=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $FAN3_PCT`
fi

# form the CPU and GPU-related temperatures
HIGHEST_CPU_TEMP=`LC_ALL=C /usr/bin/printf "%.*f\n" 1 $HIGHEST_FULL_CPU_TEMP`
HIGHEST_GPU_TEMP=`LC_ALL=C /usr/bin/printf "%.*f\n" 1 $HIGHEST_FULL_GPU_TEMP`

# determine the highest temperature
RESULT=`echo "$HIGHEST_CPU_TEMP > $HIGHEST_GPU_TEMP" | /usr/bin/bc`

if test "$RESULT" = '1'
then
	HIGHEST_TEMP_NAME="cpu"
	HIGHEST_TEMP=$HIGHEST_CPU_TEMP
	HIGHEST_FULL_TEMP=$HIGHEST_FULL_CPU_TEMP
else
	HIGHEST_TEMP_NAME="gpu"
	HIGHEST_TEMP=$HIGHEST_GPU_TEMP
	HIGHEST_FULL_TEMP=$HIGHEST_FULL_GPU_TEMP
fi

# if required, convert the temperatures
if test "$SHOW_AS_FAHRENHEIT" = 'true'
then
	HIGHEST_TEMP=`CtoF $HIGHEST_FULL_TEMP 1`
	HIGHEST_CPU_TEMP=`CtoF $HIGHEST_FULL_CPU_TEMP 1`
	HIGHEST_GPU_TEMP=`CtoF $HIGHEST_FULL_GPU_TEMP 1`
	HIGHEST_FULL_TEMP=`CtoF $HIGHEST_FULL_TEMP 1`
	LOWER_TEMP_THRESHOLD=`CtoF $LOWER_TEMP_THRESHOLD 1`
	UPPER_TEMP_THRESHOLD=`CtoF $UPPER_TEMP_THRESHOLD 1`

	TEMP_NORM_LIMIT=$F_TEMP_NORM_LIMIT
	TEMP_WARNING_LIMIT=$F_TEMP_WARNING_LIMIT
	TEMP_DANGER_LIMIT=$F_TEMP_DANGER_LIMIT
else
	TEMP_NORM_LIMIT=$C_TEMP_NORM_LIMIT
	TEMP_WARNING_LIMIT=$C_TEMP_WARNING_LIMIT
	TEMP_DANGER_LIMIT=$C_TEMP_DANGER_LIMIT
fi

if test "$SHOW_CPU_TEMP" = 'false' -a "$SHOW_GPU_TEMP" = 'false'  # show highest
then
	if test "$SHOW_TEMP_NAMES" = 'true'
	then
		DISPLAY_TEMP="${HIGHEST_TEMP_NAME} "`\
										LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_TEMP`
	else
		DISPLAY_TEMP=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_TEMP`
	fi
fi

if test "$SHOW_CPU_TEMP" = 'true' -a "$SHOW_GPU_TEMP" = 'false'  # show CPU
then
	if test "$SHOW_TEMP_NAMES" = 'true'
	then
		DISPLAY_TEMP="cpu "`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_CPU_TEMP`
	else
		DISPLAY_TEMP=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_CPU_TEMP`
	fi
fi

if test "$SHOW_CPU_TEMP" = 'false' -a "$SHOW_GPU_TEMP" = 'true'  # show GPU
then
	if test "$SHOW_TEMP_NAMES" = 'true'
	then
		DISPLAY_TEMP="gpu "`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_GPU_TEMP`
	else
		DISPLAY_TEMP=`LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_GPU_TEMP`
	fi
fi

if test "$SHOW_CPU_TEMP" = 'true' -a "$SHOW_GPU_TEMP" = 'true'  # show CPU & GPU
then
	if test "$SHOW_TEMP_NAMES" = 'true'
	then
		DISPLAY_TEMP="cpu "`\
			LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_CPU_TEMP`"${TEMP_DIVIDER}gpu "`\
			LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_GPU_TEMP`
	else
		DISPLAY_TEMP=`\
			LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_CPU_TEMP`"${TEMP_DIVIDER}"`\
			LC_ALL=C /usr/bin/printf "%.*f\n" 0 $HIGHEST_GPU_TEMP`
	fi
fi

# configure the status-icon and color used for the item(s) shown in the menu bar
COLOR="$BLK"
RESULT=`echo "$HIGHEST_FULL_TEMP > $TEMP_DANGER_LIMIT" | /usr/bin/bc`

if test "$RESULT" = '1'
then
	HIGH_TEMP_ICON="🔥"
	COLOR="$RED"
else
	RESULT=`echo "$HIGHEST_FULL_TEMP > $TEMP_WARNING_LIMIT" | /usr/bin/bc`

	if test "$RESULT" = '1'
	then
		COLOR="$ORG"
	else
		RESULT=`echo "$HIGHEST_FULL_TEMP > $TEMP_NORM_LIMIT" | /usr/bin/bc`

		if test "$RESULT" = '1'
		then
			COLOR="$GRN"
		fi
	fi
fi

if test "$SHOW_TEMPS" = 'true'
then
	if test "$HIGHEST_TEMP" = '-100.0'
	then
		TEMPS="N/A"
	else
		TEMPS="${DISPLAY_TEMP}$TEMP_LABEL_MBAR"
	fi
fi

# configure the fan-speed info to be shown in the menu bar
if test "$SHOW_FANS" = 'true'
then
	if test "$FAN1_SPEED" != "-1"
	then
		if test "$SHOW_FAN_NAMES" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}"`\
							echo "$FAN1_NAME" | /usr/bin/tr '[:upper:]' '[:lower:]'`" "
		fi

		if test "$SHOW_FAN_PCTS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN1_PCT}٪${FAN_PCT_SUFFIX}"
		fi

		if test "$SHOW_FAN_SPEEDS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN1_SPEED}"
		fi
	fi

	if test "$FAN2_SPEED" != "-1"
	then
		FAN_SPEEDS="${FAN_SPEEDS}${FAN_DIVIDER}"

		if test "$SHOW_FAN_NAMES" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}"`\
							echo "$FAN2_NAME" | /usr/bin/tr '[:upper:]' '[:lower:]'`" "
		fi

		if test "$SHOW_FAN_PCTS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN2_PCT}٪${FAN_PCT_SUFFIX}"
		fi

		if test "$SHOW_FAN_SPEEDS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN2_SPEED}"
		fi
	fi

	if test "$FAN3_SPEED" != "-1"
	then
		FAN_SPEEDS="${FAN_SPEEDS}${FAN_DIVIDER}"

		if test "$SHOW_FAN_NAMES" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}"`\
							echo "$FAN3_NAME" | /usr/bin/tr '[:upper:]' '[:lower:]'`" "
		fi

		if test "$SHOW_FAN_PCTS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN3_PCT}٪${FAN_PCT_SUFFIX}"
		fi

		if test "$SHOW_FAN_SPEEDS" = 'true'
		then
			FAN_SPEEDS="${FAN_SPEEDS}${FAN3_SPEED}"
		fi
	fi

	if test "$FAN_SPEEDS" = ''
	then
		FAN_SPEEDS=' N/A'
	fi

	if test "$SHOW_FAN_SPEEDS" = 'true'
	then
		FAN_SPEEDS="$FAN_SPEEDS rpm"
	fi
else
	FAN_SPEEDS=''
fi

if test "$SHOW_FANS" = 'true' -a "$SHOW_TEMPS" = 'true'
then
	if test "$SHOW_FAN_NAMES" = 'true'
	then
		TEMP_FAN_DIVIDER=" $TEMP_FAN_DIVIDER "
	else
		TEMP_FAN_DIVIDER=" $TEMP_FAN_DIVIDER"
	fi
else
	TEMP_FAN_DIVIDER=''
fi

# show the info that's visible in the menu bar
echo "${FAN_ICON}${HIGH_TEMP_ICON}${TEMPS}${TEMP_FAN_DIVIDER}${FAN_SPEEDS}| $FONT $COLOR"

# show a menu that can be used to change BitBar's display to fahrenheit/celcius
echo "---"

if test "$SHOW_AS_FAHRENHEIT" = 'true'
then
	echo "Show temperature(s) in celcius| bash=$BITBAR_PREF_SETTER param1=degF param2=false terminal=false refresh=true"
else
	echo "Show temperature(s) in fahrenheit| bash=$BITBAR_PREF_SETTER param1=degF param2=true terminal=false refresh=true"
fi

# show a menu that can be used to change what's shown in BitBar's display
echo "---"
echo "In the menu bar ...| $GRY"

if test "$SHOW_FANS" = 'false' -a "$SHOW_TEMPS" = 'true'
then
	echo "Show only the fan speed(s)| bash=$BITBAR_PREF_SETTER param1=fans param2=true param3=temps param4=false terminal=false refresh=true"
fi

if test "$SHOW_FANS" = 'true' -a "$SHOW_TEMPS" = 'false'
then
	echo "Show only the temperature(s)| bash=$BITBAR_PREF_SETTER param1=fans param2=false param3=temps param4=true terminal=false refresh=true"
fi

if test "$SHOW_FANS" = 'true' -a "$SHOW_TEMPS" = 'true'
then
	echo "Show only the temperature(s)| bash=$BITBAR_PREF_SETTER param1=fans param2=false param3=temps param4=true terminal=false refresh=true"
	echo "Show only the fan speed(s)| bash=$BITBAR_PREF_SETTER param1=fans param2=true param3=temps param4=false terminal=false refresh=true"
else
	echo "Show both the temperature(s) and the fan speed(s)| bash=$BITBAR_PREF_SETTER param1=fans param2=true param3=temps param4=true terminal=false refresh=true"
fi

if test "$SHOW_TEMPS" = 'true'
then
	echo ' |strip=false'

	if test "$SHOW_CPU_TEMP" = 'false' -a "$SHOW_GPU_TEMP" = 'false'
	then
		echo "Showing only the the hightest temperature"
		echo "Show only the CPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=true terminal=false refresh=true"
		echo "Show only the GPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=false terminal=false refresh=true"
		echo "Show both the CPU and GPU-related temperatures| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=true terminal=false refresh=true"
	fi

	if test "$SHOW_CPU_TEMP" = 'false' -a "$SHOW_GPU_TEMP" = 'true'
	then
		echo "Showing only the highest GPU-related temperature"
		echo "Show only the CPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=true terminal=false refresh=true"
		echo "Show only the highest temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=false terminal=false refresh=true"
		echo "Show both the CPU and GPU-related temperatures| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=true terminal=false refresh=true"
	fi

	if test "$SHOW_CPU_TEMP" = 'true' -a "$SHOW_GPU_TEMP" = 'false'
	then
		echo "Showing only the highest CPU-related temperature"
		echo "Show only the GPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=false terminal=false refresh=true"
		echo "Show only the highest temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=false terminal=false refresh=true"
		echo "Show both the CPU and GPU-related temperatures| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=true terminal=false refresh=true"
	fi

	if test "$SHOW_CPU_TEMP" = 'true' -a "$SHOW_GPU_TEMP" = 'true'
	then
		echo "Showing both the highest CPU and GPU-related temperatures"
		echo "Show only the CPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=true terminal=false refresh=true"
		echo "Show only the GPU-related temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=true param3=cpu param4=false terminal=false refresh=true"
		echo "Show only the highest temperature| bash=$BITBAR_PREF_SETTER param1=gpu param2=false param3=cpu param4=false terminal=false refresh=true"
	fi
fi

if test "$SHOW_FANS" = 'true' -o "$SHOW_TEMPS" = 'true'
then
	echo ' |strip=false'
fi

if test "$SHOW_TEMPS" = 'true'
then
	if test "$SHOW_TEMP_NAMES" = 'true'
	then
		echo "Hide the temperature name(s)| bash=$BITBAR_PREF_SETTER param1=tname param2=false terminal=false refresh=true"
	else
		echo "Show the temperature name(s)| bash=$BITBAR_PREF_SETTER param1=tname param2=true terminal=false refresh=true"
	fi
fi

if test "$SHOW_FANS" = 'true'
then
	if test "$SHOW_FAN_NAMES" = 'true'
	then
		echo "Hide the fan name(s)| bash=$BITBAR_PREF_SETTER param1=fname param2=false terminal=false refresh=true"
	else
		echo "Show the fan name(s)| bash=$BITBAR_PREF_SETTER param1=fname param2=true terminal=false refresh=true"
	fi

	echo ' |strip=false'

	if test "$SHOW_FAN_SPEEDS" = 'true'
	then
		if test "$SHOW_FAN_PCTS" = 'true'
		then
			echo "Hide the fan speed(s)| bash=$BITBAR_PREF_SETTER param1=fspeed param2=false terminal=false refresh=true"
		else
			echo "Show only the fan-speed percentage(s)| bash=$BITBAR_PREF_SETTER param1=fspeed param2=false param3=fpercent param4=true terminal=false refresh=true"
			echo "Show both the fan speed(s) and fan-speed percentage(s)| bash=$BITBAR_PREF_SETTER param1=fspeed param2=true param3=fpercent param4=true terminal=false refresh=true"
		fi
	fi

	if test "$SHOW_FAN_PCTS" = 'true'
	then
		if test "$SHOW_FAN_SPEEDS" = 'true'
		then
			echo "Hide the fan speed percentage(s)| bash=$BITBAR_PREF_SETTER param1=fpercent param2=false terminal=false refresh=true"
		else
			echo "Show only the fan speed(s)| bash=$BITBAR_PREF_SETTER param1=fspeed param2=true param3=fpercent param4=false terminal=false refresh=true"
			echo "Show both the fan speed(s) and fan-speed percentage(s)| bash=$BITBAR_PREF_SETTER param1=fspeed param2=true param3=fpercent param4=true terminal=false refresh=true"
		fi
	fi
fi

# if applicable, show errors
if test "$HIGHEST_TEMP" = '-100.0' -o "$SHOW_FANS" = 'true' -a \
		  \( "$NUM_FANS" = '' -o "$FAN_SPEEDS" = '' \)
then
	echo "---"
fi

if test "$HIGHEST_CPU_TEMP" = '-100.0'
then
	echo "WARNING: no CPU-related temperatue reading is available| $ORG"
fi

if test "$HIGHEST_GPU_TEMP" = '-100.0'
then
	echo "WARNING: no GPU-related temperatue reading is available| $ORG"
fi

if test "$HIGHEST_TEMP" = '-100.0'
then
	echo "ERROR: no CPU or GPU-related temperature reading is available| $RED"
fi

if test "$NUM_FANS" = ''
then
	echo "ERROR: the number of fans could not be determined| $RED"
fi

if test "$SHOW_FANS" = 'true' -a "$FAN_SPEEDS" = ''
then
	echo "ERROR: no fan speed(s) are available| $RED"
fi

# show detailed info
if test "$HIGHEST_TEMP" != '-100.0'
then
	echo "---"
	echo "Current Temperature Info:| $LT_GRY"

	if test "$HIGHEST_TEMP" != '-100.0'
	then
		echo "Highest Temperature = ${HIGHEST_TEMP}${TEMP_LABEL}| $GRY"
	fi

	if test "$HIGHEST_CPU_TEMP" != '-100.0'
	then
		echo "Highest CPU-related Temperature = ${HIGHEST_CPU_TEMP}${TEMP_LABEL}| $GRY"
	fi

	if test "$HIGHEST_GPU_TEMP" != '-100.0'
	then
		echo "Highest GPU-related Temperature = ${HIGHEST_GPU_TEMP}${TEMP_LABEL}| $GRY"
	fi
fi

echo "---"
echo "Current Fan Control Settings:| $LT_GRY"

if test ! -f "$PREFS_FILE"
then
	echo "WARNING: no Fan Control preferences exist ($PREFS_FILE)| $ORG"
fi

if test "$LOWER_TEMP_THRESHOLD" != ''
then
	echo "Lower Temperature Threshold = ${LOWER_TEMP_THRESHOLD}${TEMP_LABEL}| $GRY"
else
	echo "WARNING: no Fan Control 'Lower Temp Threshold' preference exists| $ORG"
fi

if test "$UPPER_TEMP_THRESHOLD" != ''
then
	echo "Upper Temperature Threshold = ${UPPER_TEMP_THRESHOLD}${TEMP_LABEL}| $GRY"
else
	echo "WARNING: no Fan Control 'Upper Temp Threshold' preference exists| $ORG"
fi

if test "$SHOW_AS_FAHRENHEIT_FC" = 'false' -o "$SHOW_AS_FAHRENHEIT_FC" = 'true'
then
	echo "Show temperatures in degress Fahrenheit = ${SHOW_AS_FAHRENHEIT_FC}| $GRY"
else
	echo "WARNING: no Fan Control 'Show temperatures in degress Fahrenheit' preference exists| $ORG"
fi

if test "$FAN1_MIN_RPM" != ''
then
	echo "Min. ${FAN1_NAME} Fan Speed = ${FAN1_MIN_RPM} rpm| $GRY"
else
	echo "WARNING: no Fan Control 'Min. Fan Speed' preference exists| $ORG"
fi

if test "$FAN2_MIN_RPM" != ''
then
	echo "Min. ${FAN2_NAME} Fan Speed = ${FAN2_MIN_RPM} rpm| $GRY"
fi

if test "$FAN3_MIN_RPM" != ''
then
	echo "Min. ${FAN3_NAME} Fan Speed = ${FAN3_MIN_RPM} rpm| $GRY"
fi

echo '---'
echo "Misc. Info:| $LT_GRY"
echo "Number of fans = ${NUM_FANS}| $GRY"

if test $NUM_FANS -gt 0
then
	echo "Max. $FAN1_NAME fan speed = $FAN1_MAX_RPM rpm| $GRY"
fi

if test $NUM_FANS -gt 1
then
	echo "Max. $FAN2_NAME fan speed = $FAN2_MAX_RPM rpm| $GRY"
fi

if test $NUM_FANS -gt 2
then
	echo "Max. $FAN3_NAME fan speed = $FAN3_MAX_RPM rpm| $GRY"
fi

echo ' |strip=false'
echo "Normal temperature threshold = ${TEMP_NORM_LIMIT}${TEMP_LABEL}| $GRN"
echo "Medium temperature threshold = ${TEMP_WARNING_LIMIT}${TEMP_LABEL}| $ORG"
echo "High temperature threshold = ${TEMP_DANGER_LIMIT}${TEMP_LABEL}| $RED"
# echo ' |strip=false'
# echo "SHOW_FANS = ${SHOW_FANS}| $GRY"
# echo "SHOW_TEMPS = ${SHOW_TEMPS}| $GRY"
# echo ' |strip=false'
# echo "SHOW_CPU_TEMP = ${SHOW_CPU_TEMP}| $GRY"
# echo "SHOW_GPU_TEMP = ${SHOW_GPU_TEMP}| $GRY"
# echo ' |strip=false'
# echo "SHOW_FAN_NAMES = ${SHOW_FAN_NAMES}| $GRY"
# echo "SHOW_TEMP_NAMES = ${SHOW_TEMP_NAMES}| $GRY"
#echo ' |strip=false'
#echo "FAN1_MAX_RPM = ${FAN1_MAX_RPM}| $GRY"
#echo "FAN1_SPEED = ${FAN1_SPEED}| $GRY"
#echo "FAN1_PCT = ${FAN1_PCT}| $GRY"
#echo "FAN2_MAX_RPM = ${FAN2_MAX_RPM}| $GRY"
#echo "FAN2_SPEED = ${FAN2_SPEED}| $GRY"
#echo "FAN2_PCT = ${FAN2_PCT}| $GRY"
#echo "FAN3_MAX_RPM = ${FAN3_MAX_RPM}| $GRY"
#echo "FAN3_SPEED = ${FAN3_SPEED}| $GRY"
#echo "FAN3_PCT = ${FAN3_PCT}| $GRY"
