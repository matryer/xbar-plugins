#!/bin/bash

# Get current Music rating, compatible with Mac OS 10.15 and old versions.
#
# based on iTunes script by 
# Sebastian Winkler (sw2@github), Dan Turkel (daturkel@gmail.com) 

# metadata
# <bitbar.title>Music Rating</bitbar.title>
# <bitbar.version>v0.1</bitbar.version>
# <bitbar.author>Weibing Chen, Sebastian Winkler, Dan Turkel</bitbar.author>
# <bitbar.author.github>weibingchen17</bitbar.author.github>
# <bitbar.desc>Display currently rating from Music song and modify it.</bitbar.desc>
# <bitbar.image>http://www.mindfuckbox.com/wp-content/uploads/2016/01/iTunes_Rating.png</bitbar.image>

vercomp () {
    if [[ $1 == "$2" ]]; then
        return 0
    fi
    local IFS=.
    local i ver1=($1) ver2=($2)
    # fill empty fields in ver1 with zeros
    for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)); do
        ver1[i]=0
    done
    for ((i=0; i<${#ver1[@]}; i++)); do
        if [[ -z ${ver2[i]} ]]; then
            # fill empty fields in ver2 with zeros
            ver2[i]=0
        fi
        if ((10#${ver1[i]} < 10#${ver2[i]})); then
            return 1
        fi
        if ((10#${ver1[i]} > 10#${ver2[i]})); then
            return 2
        fi
    done
    return 0
}

MusicApp="Music"
SysVersion=$(sw_vers -productVersion)
MacOSCatalina="10.15"
vercomp $MacOSCatalina "$SysVersion"
if (($? >  1)); then
    MusicApp="iTunes"
fi

LAUNCH="tell application \"$MusicApp\" to activate"
QUERY_RUNNING="application \"$MusicApp\" is running"
SET_TO_00="tell application \"$MusicApp\" to set rating of current track to 00"
SET_TO_20="tell application \"$MusicApp\" to set rating of current track to 20"
SET_TO_40="tell application \"$MusicApp\" to set rating of current track to 40"
SET_TO_60="tell application \"$MusicApp\" to set rating of current track to 60"
SET_TO_80="tell application \"$MusicApp\" to set rating of current track to 80"
SET_TO_100="tell application \"$MusicApp\" to set rating of current track to 100"
GET_NAME="tell application \"$MusicApp\" to name of current track as string"
GET_ARTIST="tell application \"$MusicApp\" to artist of current track as string"
GET_RATING="tell application \"$MusicApp\" to rating of current track as string"

if [ "$1" = 'launch' ]; then
  osascript -e "$LAUNCH"
  exit
fi

if [ "$(osascript -e "$QUERY_RUNNING")" = "false" ]; then
  echo "♫"
  echo "---"
  echo "Music is not running"
  echo "Launch Music | bash='$0' param1=launch terminal=false"
  exit
fi

case $1 in 
    'zero')
        osascript -e "$SET_TO_00"
        exit
        ;;
    'one')
        osascript -e "$SET_TO_20"
        exit
        ;;
    'two')
        osascript -e "$SET_TO_40"
        exit
        ;;
    'three')
        osascript -e "$SET_TO_60"
        exit
        ;;
    'four')
        osascript -e "$SET_TO_80"
        exit
        ;;
    'five')
        osascript -e "$SET_TO_100"
        exit
        ;;
esac

rating_icon_black="★"
rating_icon_white="☆"

track=$(osascript -e "$GET_NAME" 2>/dev/null);
artist=$(osascript -e "$GET_ARTIST" 2>/dev/null);
rating=$(osascript -e "$GET_RATING" 2>/dev/null);

case $rating in
    00)
        echo $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
    20)
        echo $rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
    40)
        echo $rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white 
        ;;
    60)
        echo $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white 
        ;;
    80)
        echo $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white 
        ;;
    100)
        echo $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black
        ;;
    *)
        echo $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
esac

echo "---"

case "$0" in
  *\ * )
   echo "Your script path | color=#ff0000"
   echo "($0) | color=#ff0000"
   echo "has a space in it, which BitBar does not support. | color=#ff0000"
   echo "Play/Pause/Next/Previous buttons will not work. | color=#ff0000"
  ;;
esac

echo "$artist -- $track"

case $rating in
    00)
        echo Rating: $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
    20)
        echo Rating: $rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
    40)
        echo Rating: $rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white 
        ;;
    60)
        echo Rating: $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white 
        ;;
    80)
        echo Rating: $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white 
        ;;
    100)
        echo Rating: $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black
        ;;
    *)
        echo Rating: $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white
        ;;
esac

echo '---'
echo 'Rerate'
echo "$rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white | bash='$0' param1=zero refresh=true terminal=false "
echo "$rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white $rating_icon_white | bash='$0' param1=one refresh=true terminal=false "
echo "$rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white $rating_icon_white | bash='$0' param1=two refresh=true terminal=false "
echo "$rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white $rating_icon_white | bash='$0' param1=three refresh=true terminal=false "
echo "$rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_white | bash='$0' param1=four refresh=true terminal=false "
echo "$rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black $rating_icon_black | bash='$0' param1=five refresh=true terminal=false "
