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


# start outputting information to bitbar
if [ "$playing" = "" ] && [ "$paused" = "" ]; then
	# nothing is even paused
	echo "🙉 No music playing | color=gray"
else
	# something is playing or is paused
	track=""
	artist=""

	if [ "$playing" = "" ]; then
		echo "$paused is paused | color=#888888"
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
	
	# output the track and artist
	track=$(osascript -e "tell application \"$app\" to $track_query")
	artist=$(osascript -e "tell application \"$app\" to $artist_query")

	echo "$track | length=40" | awk -F '\ -' '{print $1}'
	echo "---"
	echo "$artist"

	if [ "$playing" != "" ]; then
		echo "Now playing on $app | color=gray bash='$0' param1=open param2=$app terminal=false"
		echo "---"
		echo "⏸ Pause | bash='$0' param1=pause param2=$app refresh=true terminal=false"
	else
		echo "▶️ Play | bash='$0' param1=play param2=$app refresh=true terminal=false"
	fi

	echo "⏭ Next | bash='$0' param1=next param2=$app refresh=true terminal=false"
	echo "⏮ Previous | bash='$0' param1=previous param2=$app refresh=true terminal=false"
fi

# add an Open option for each service
echo "---"
for i in "${apps[@]}"; do
	echo "Open $i | bash='$0' param1=open param2=$i terminal=false"
done
