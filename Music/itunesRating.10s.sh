#!/bin/bash

# Get current iTunes rating
#
# Sebastian Winkler
#
# based on iTunes script by Dan
# Turkel (daturkel@gmail.com) 

# metadata
# <bitbar.title>iTunes Rating</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Sebastian Winkler, Dan Turkel</bitbar.author>
# <bitbar.author.github>sw82</bitbar.author.github>
# <bitbar.desc>Display currently rating from iTunes song and modify it.</bitbar.desc>
# <bitbar.image>http://www.mindfuckbox.com/wp-content/uploads/2016/01/iTunes_Rating.png</bitbar.image>


if [ "$1" = 'launch' ]; then
  osascript -e 'tell application "iTunes" to activate'
  exit
fi

if [ "$(osascript -e 'application "iTunes" is running')" = "false" ]; then
  echo "♫"
  echo "---"
  echo "iTunes is not running"
  echo "Launch iTunes | bash='$0' param1=launch terminal=false"
  exit
fi

if [ "$1" = 'zero' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 00'
  exit
fi

if [ "$1" = 'one' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 20'
  exit
fi

if [ "$1" = 'two' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 40'
  exit
fi

if [ "$1" = 'three' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 60'
  exit
fi

if [ "$1" = 'four' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 80'
  exit
fi

if [ "$1" = 'five' ]; then
  osascript -e 'tell application "iTunes" to set rating of current track to 100'
  exit
fi

rating_icon_black="★"
rating_icon_white="☆"

track=$(osascript -e 'tell application "iTunes" to name of current track as string');
artist=$(osascript -e 'tell application "iTunes" to artist of current track as string');
rating=$(osascript -e 'tell application "iTunes" to rating of current track as string');

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