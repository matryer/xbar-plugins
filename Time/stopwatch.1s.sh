#!/bin/bash

# <bitbar.title>Stopwatch</bitbar.title>
# <bitbar.version>1.0</bitbar.version>
# <bitbar.author>Tim Baumgard</bitbar.author>
# <bitbar.author.github>tbaumgard</bitbar.author.github>
# <bitbar.desc>Simple stopwatch for BitBar.</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/tbaumgard/tbaumgard.github.io/master/_/img/bitbar-stopwatch.png</bitbar.image>

### Configuration

MAX_SPLITS=25
START_FILE="$HOME/.bitbar-stopwatch-start"
PAUSE_FILE="$HOME/.bitbar-stopwatch-pause"
SPLIT_FILE="$HOME/.bitbar-stopwatch-split"
SPLITS_FILE="$HOME/.bitbar-stopwatch-splits"
START_TEXT="Start"
STOP_TEXT="Stop"
SPLIT_TEXT="Split"
RESET_TEXT="Reset"

### Functions

function stopwatch-print-splits {
	local COUNTER
	local CLOCK

	if [ ! -n "$1" ]; then
		return
	fi

	COUNTER=1
	echo "---"

	for SPLIT in $1; do
		CLOCK="$(stopwatch-print-clock "$SPLIT")"
		echo "$COUNTER. $CLOCK"
		COUNTER=$((COUNTER + 1))
	done
}

function stopwatch-print-clock {
	local NUM_SECONDS=$(($1))
	local HOURS=$((NUM_SECONDS / 3600))
	local MINUTES=$((NUM_SECONDS % 3600 / 60))
	local SECONDS=$((NUM_SECONDS % 3600 % 60))

	printf "%02d:%02d:%02d" $HOURS $MINUTES $SECONDS
}

### Main

START_TIME="$(cat "$START_FILE" 2> /dev/null)"
PAUSE_TIME="$(cat "$PAUSE_FILE" 2> /dev/null)"
SPLIT_TIME="$(cat "$SPLIT_FILE" 2> /dev/null)"
SPLITS="$(cat "$SPLITS_FILE" 2> /dev/null)"

if [ "$1" == "start" ]; then
	date +%s > "$START_FILE"
	exit
elif [ "$1" == "pause" ]; then
	ELAPSED_TIME=$(($(date +%s) - START_TIME))
	echo $ELAPSED_TIME > "$PAUSE_FILE"
	exit
elif [ "$1" == "resume" ]; then
	OFFSET_START_TIME=$(($(date +%s) - PAUSE_TIME))
	echo $OFFSET_START_TIME > "$START_FILE"
	rm "$PAUSE_FILE"
	exit
elif [ "$1" == "split" ]; then
	NOW="$(date +%s)"

	if [ -n "$SPLIT_TIME" ]; then
		ELAPSED_TIME=$((NOW - SPLIT_TIME))
	else
		ELAPSED_TIME=$((NOW - START_TIME))
	fi

	echo "$NOW" > "$SPLIT_FILE"

	if [ -n "$SPLITS" ]; then
		printf "%s\n%d\n" "$SPLITS" $ELAPSED_TIME | tail -n "$MAX_SPLITS" > "$SPLITS_FILE"
	else
		printf "%d\n" $ELAPSED_TIME > "$SPLITS_FILE"
	fi

	exit
elif [ "$1" == "reset" ]; then
	rm "$START_FILE"
	rm "$PAUSE_FILE"
	rm "$SPLIT_FILE"
	rm "$SPLITS_FILE"
	exit
fi

if [ -n "$PAUSE_TIME" ]; then
	CLOCK="$(stopwatch-print-clock "$PAUSE_TIME")"

	echo "◎ $CLOCK"
	echo "---"
	echo "$START_TEXT| bash='$0' param1=resume terminal=false refresh=true"
	echo "$RESET_TEXT| bash='$0' param1=reset terminal=false refresh=true"
	stopwatch-print-splits "$SPLITS"
elif [ -n "$START_TIME" ]; then
	ELAPSED_TIME=$(($(date +%s) - START_TIME))
	CLOCK="$(stopwatch-print-clock "$ELAPSED_TIME")"

	echo "◉ $CLOCK"
	echo "---"
	echo "$STOP_TEXT| bash='$0' param1=pause terminal=false refresh=true"

	if [ "$MAX_SPLITS" -gt 0 ]; then
		echo "$SPLIT_TEXT| bash='$0' param1=split terminal=false refresh=true"
	fi

	stopwatch-print-splits "$SPLITS"
else
	echo "◎ 00:00:00"
	echo "---"
	echo "$START_TEXT| bash='$0' param1=start terminal=false refresh=true"
fi
