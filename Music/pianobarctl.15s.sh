#!/bin/bash
# <bitbar.title>PianoBar Control</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>danlogan</bitbar.author>
# <bitbar.author.github>danlogan</bitbar.author.github>
# <bitbar.desc>Simple pianobar controls from the menu bar.</bitbar.desc>
# <bitbar.image>https://i.imgur.com/xWjlIDr.png</bitbar.image>
# <bitbar.dependencies></bitbar.dependencies>
# <bitbar.abouturl></bitbar.abouturl>

# pianobar should not already be running when you start this script

# pianobar must be installed and updated for this script to run
#   brew install pianobar 
# you must have logged in to pianobar at least once outside this script and set the start station so it autoplays on launch
# user and password must be set in your pianobar config (~/.config/pianobar/config)
# your pianobar config must have the fifo line enabled and set to file "ctl" file path set below.  The fifo file will be created if it doesnt exist
#   e.g. fifo = ~/.config/pianobar/ctl
#
# if you want to get the current song to display in the pianobar menu you must do two things.  
# First, create a file called nowplaying in the pianobar config directory
#   touch ~/.config/pianobar/nowplaying
# Second uncomment the eventcmd in your pianobar config 
#   e.g. event_command = ~/.config/pianobar/eventcmd.sh
# and create eventcmd.sh a file with the following contents in to your config folder.  
# make it executable "chmod +x ~/.config/pianobar/eventcmd.sh" 
#
#  touch ~/.config/pianobar/eventcmd.sh
#  chmod +x ~/.config/pianobar/eventcmd.sh
#
##--eventcmd.sh start--
#  #!/bin/bash
#
#  # create variables
#  while read L; do
#  	k="`echo "$L" | cut -d '=' -f 1`"
#  	v="`echo "$L" | cut -d '=' -f 2`"
#  	export "$k=$v"
#  done < <(grep -e '^\(title\|artist\|album\|stationName\|songStationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin) # don't overwrite $1...
#  
#  case "$1" in
#  	songstart)
#     echo "$title -- $artist" > ~/.config/pianobar/nowplaying
#    ;;
#  
#    *)
#      echo "nothing yet" > ~/.config/pianobar/nowplaying
#    ;;
#  esac
##--eventcmd.sh end--

# troubleshooting
# - make sure pianobar is up to date
# - make sure this file is executable
#     chmod +x pianobarctl.15s.sh
# - if you are not getting results in the script double check your config
# - check your running prcesses for "pianobar" and "screen" and kill them if they are running

# create the fifo file for pianobar if it doesnt exist
# allow for pause/unpause, next, quit, and launch
# read nowplaying if eventcmd is configured
# refresh every 15s
[ ! -e ~/.config/pianobar/ctl ] && mkfifo ~/.config/pianobar/ctl
echo "$1"

case "$1" in

  togglepause)
    echo -n 'p' > ~/.config/pianobar/ctl
    ;;

  next)
    echo -n 'n' > ~/.config/pianobar/ctl
    ;;

  quit)
    echo -n 'q' > ~/.config/pianobar/ctl
    sleep 2
    ;;

  launch)
    
    num=$(pgrep "pianobar")
    
    if [ -z "$num" ] 
      then
        screen -dmS pb /usr/local/bin/pianobar
    fi
    sleep 5
    ;;

  *)
    #echo -n "unknown"
    ;;
esac

[ -e ~/.config/pianobar/nowplaying ] && nowplaying=$(<~/.config/pianobar/nowplaying)

num=$(pgrep "pianobar")
# (ps aux | grep -v grep | grep -v pianobarctl.15s.sh | grep -ci "pianobar") 
# echo $num
if [ -z "$num" ] 
  then
    state="off";
else
    state="on";
fi

echo pianobar #$state

echo ---
[ $state = "on" ] && echo "$nowplaying" 
[ $state = "off" ] && echo "Launch | bash=\"$0\" param1=launch terminal=false refresh=true"
[ $state = "on" ] && echo "Pause / Unpause | bash=\"$0\" param1=togglepause terminal=false"
[ $state = "on" ] && echo "Next | bash=\"$0\" param1=next terminal=false"
[ $state = "on" ] && echo "Quit | bash=\"$0\" param1=quit terminal=false refresh=true"
