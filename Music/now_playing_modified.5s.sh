#!/bin/bash

# <bitbar.title>Now playing</bitbar.title>
# <bitbar.version>v1.1</bitbar.version>
# <bitbar.author>Adam Kenyon</bitbar.author>
# <bitbar.author.github>adampk90</bitbar.author.github>
# <bitbar.desc>Shows and controls the music that is now playing. Currently supports Spotify, iTunes, and Vox.</bitbar.desc>
# <bitbar.image>https://pbs.twimg.com/media/CbKmTS7VAAA84VS.png:small</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

# first, determine if there's an app that's playing or paused
apps=(Spotify iTunes Vox)
playing=""
paused=""

for i in "${apps[@]}"; do
	# is the app running?
	app_state=$(osascript -e "application \"$i\" is running")

	# shellcheck disable=SC2181
	if [ "$?" != "0" ]; then
		# just exit if there was an error determining the app's state
		# (the app might be in the middle of quitting)
		exit
	fi

	if [ "$app_state" = "true" ] && [ "$track" = "" ]; then
		# yes, it's running
		# is it playing music currently?
		app_playing=$(osascript -e "tell application \"$i\" to player state as string")
		if [ "$app_playing" = "paused" ] || [ "$app_playing" = "0" ]; then
			# nope, it's paused
			paused="$i"
		elif [ "$app_playing" = "playing" ] || [ "$app_playing" = "1" ]; then
			# yes, it's playing
			playing="$i"
		fi
	fi
done


# open a specified app
if [ "$1" = "open" ]; then
	osascript -e "tell application \"$2\" to activate"
	exit
fi
# play/pause
if [ "$1" = "play" ] || [ "$1" = "pause" ]; then
	osascript -e "tell application \"$2\" to $1"
	exit
fi
# next/previous
if [ "$1" = "next" ] || [ "$1" = "previous" ]; then
	osascript -e "tell application \"$2\" to $1 track"
	# tell spotify to hit "Previous" twice so it actually plays the previous track
	# instead of just starting from the beginning of the current one
	if [ "$playing" = "Spotify" ] && [ "$1" = "previous" ]; then
		osascript -e "tell application \"$2\" to $1 track"
	fi
	osascript -e "tell application \"$2\" to play"
	exit
fi
# close a specified app
if [ "$1" = "close" ]; then
    osascript -e "tell application \"$2\" to quit"
    exit
fi

# start outputting information to bitbar
if [ "$playing" = "" ] && [ "$paused" = "" ]; then
#   nothing is even paused
#   echo "🙉 No music playing | color=gray"
    echo " | color=clear"
else
	# something is playing or is paused
	track=""
	artist=""

	if [ "$playing" = "" ]; then
		echo "paused | color=#888888"
        echo "---"
		app="$paused"
	else
		app="$playing"
	fi

	track_query="name of current track"
	artist_query="artist of current track"
	# Vox uses a different syntax for track and artist names
   if [ "$app" = "Vox" ]; then
       track_query="track"
       artist_query="artist"
   fi
	
	# get track
	track=$(osascript -e "tell application \"$app\" to $track_query")

    # only use the track title that comes be before a '(' or '-' or '['
    track=${track%(*}
    track=${track%-*}
    track=${track%[*}

    # get artist
	artist=$(osascript -e "tell application \"$app\" to $artist_query")

#    Replace line 113 with the code below if you want only 30 characters of the song name to be displayed in the menu bar
#    echo "$track | color=#C70039 length=30"

    echo "$track | color=#C70039"

	echo "---"
	
	if [ "$playing" != "" ]; then
		echo "---"
		echo "II  Pause | bash='$0' param1=pause param2=$app refresh=true terminal=false"
	else
		echo ">  Play | bash='$0' param1=play param2=$app refresh=true terminal=false"
	fi

    # IMPORTANT: EXECUTION OF FUNCTIONS. PARAMETERS ARE $1 AND $2 on lines 44-65
	echo "›› Next | bash='$0' param1=next param2=$app refresh=true terminal=false"
	echo "‹‹ Previous | bash='$0' param1=previous param2=$app refresh=true terminal=false"
	
	echo "---"
	
    # reset the name of $track to the full name for the drop down menu
    track=$(osascript -e "tell application \"$app\" to $track_query")
    echo "$track"
	echo "$artist"

fi

for i in "${apps[@]}"; do
    # is the app running?
    app_state=$(osascript -e "application \"$i\" is running")

    if [ "$?" != "0" ]; then
        # just exit if there was an error determining the app's state
        # (the app might be in the middle of quitting)
        exit
    fi

    if [ "$app_state" = "true" ]; then
        echo "---"
        echo "Open $app | color=#1db954 bash='$0' param1=open param2=$app terminal=false"
        echo "Quit | color=red bash='$0' param1=close param2=$app terminal=false"
    fi
done

