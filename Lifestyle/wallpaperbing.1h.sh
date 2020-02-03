#!/bin/bash

# <bitbar.title>Bing Wallpapers</bitbar.title>
# <bitbar.version>v2.0</bitbar.version>
# <bitbar.author>Tok1</bitbar.author>
# <bitbar.author.github>Tokfrans03</bitbar.author.github>
# <bitbar.desc>A new wallpaper from Bing every hour</bitbar.desc>
# <bitbar.dependencies>JQ</bitbar.dependencies>

export PATH="/usr/local/bin:$PATH"

JQ=$(command -v jq)

imageurls=(/tmp/imageurls.txt)

first_time=$(defaults read wallpaperbing path 2>&1 | grep exist)

# If it's the first time we create a domain

if [ ! "$first_time" = '' ]; then 

    defaults write wallpaperbing path /tmp/wallpaper.jpg

    defaults write wallpaperbing setting today

fi

readsetting=$(defaults read wallpaperbing setting)

defaultspath=$(defaults read wallpaperbing path)

# Create a random number for random URL selection

random=$((1 + RANDOM % 8))

# check if JQ is installed

if [ ! -e "$JQ" ]; then

    echo "Please install JQ with brew install JQ"

    echo ---

    echo "Install JQ | bash=brew param1=install param2=jq terminal=true"
    
    echo ---

    echo "Update | refresh=true"

    exit

fi

# Download the json with all the image URLs

json=$( (curl -s "https://www.bing.com/HPImageArchive.aspx?format=js&idx=0&n=8") | $JQ '.images')

if [ "$1" = '' ]; then

    #Bing logo

    echo "| templateImage=iVBORw0KGgoAAAANSUhEUgAAAAwAAAAPCAQAAAB66ObCAAAAr0lEQVR4AXWNM0LHARzFP9l2nSCObR0g2xfItrV2gbzEuZZca+25lrDl+tv88Wu+B09sUE88MtFhQUez8sLCgNqinxDClRa95LLHBHnE4CP9OMDCDxfMU0CakKPRVVvQcC1chNPBJUZHbxYuIIpR/oSLP2oJpppTD9gP6PlghFJ20biGBo4ogzfW2OEbi0sf6CYW4JwPz/CLRTIAAHLYQOsAOKGAQAQSySQvzJGIQKxs7lzLIn88zwAAAABJRU5ErkJggg=="
    echo ---
    
    
    echo "Update | refresh=true"
    
    echo ---
    
    echo "Options"
    echo "--Today | bash=$0 param1='set' param2='today' terminal=false refresh=true"
    echo "--Random | bash=$0 param1='set' param2='random' terminal=false refresh=true"
    


    if [ "$readsetting" = "today" ]; then

        echo "Current setting: Today"

        $0 today

    else
    
      echo "Current setting: Random"

        $0 random

    fi

fi

if [ "$1" = 'set' ]; then

    if [ "$2" = 'today' ]; then

        defaults write wallpaperbing setting today

    fi

    if [ "$2" = 'random' ]; then

        defaults write wallpaperbing setting random

    fi
fi


if [ "$1" = 'today' ]; then

    # Decide path for the wallpaper 

    if [ "$defaultspath" = "/tmp/wallpaper1.jpg" ]; then

        wallpaperpath=("/tmp/wallpaper1.jpg")

        defaults write wallpaperbing path /tmp/wallpaper.jpg

        rm /tmp/wallpaper.jpg 2>&-

    else

        wallpaperpath=("/tmp/wallpaper.jpg")

        defaults write wallpaperbing path /tmp/wallpaper1.jpg

        rm /tmp/wallpaper1.jpg 2>&-

        # echo aaaaa

        echo "${wallpaperpath[0]}"

    fi

    # Create setting to remember

    defaults write wallpaperbing setting today

    echo "$json" | $JQ '.[0]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/ > /tmp/imageurls.txt

    curl -s -L "$(cat "${imageurls[0]}")" -o "${wallpaperpath[0]}"

    # Get comment

    Comment=$(echo "$json" | $JQ '.[0]' | $JQ '.copyright' | sed s/'"'// | sed s/'"'//)

    echo "$Comment"

    # Set dummy image as wallpaper so Finder will change the wallpaper to the pic that we want

    # No langer needed as we choose a new name every time

    # echo "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=" | base64 -D -o "/tmp/dummyimg.jpg"

    # osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/private/tmp/dummyimg.jpg"'

    # Set image as wallpaper

    if [ "${wallpaperpath[0]}" == "/tmp/wallpaper.jpg" ]; then
    
    osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper.jpg"'

    else

    osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper1.jpg"'

fi

# Cleanup
 
# rm /tmp/imageurls.txt

exit
fi

if [ "$1" = 'random' ]; then

    # Decide path for the wallpaper 

    if [ "$defaultspath" = "/tmp/wallpaper1.jpg" ]; then

        wallpaperpath=("/tmp/wallpaper1.jpg")

        defaults write wallpaperbing path /tmp/wallpaper.jpg

        rm /tmp/wallpaper.jpg 2>&-

    else

        wallpaperpath=("/tmp/wallpaper.jpg")

        defaults write wallpaperbing path /tmp/wallpaper1.jpg

        rm /tmp/wallpaper1.jpg 2>&-

    fi

    # Create setting to remember

    defaults write wallpaperbing setting random

    # Add URLs to a file and add bing to the beginning 

    echo "$json" | $JQ '.[0]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/ > /tmp/imageurls.txt
    {
    echo "$json" | $JQ '.[1]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/ 
    echo "$json" | $JQ '.[3]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    echo "$json" | $JQ '.[2]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    echo "$json" | $JQ '.[4]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    echo "$json" | $JQ '.[5]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    echo "$json" | $JQ '.[6]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    echo "$json" | $JQ '.[7]' | $JQ '.url' | sed s/'"'// | sed s/'"'// | sed s/'\/'/'https:\/\/bing.com\/'/
    } >> /tmp/imageurls.txt



    # Select a URL from a random line 

    randomurl=$(< "${imageurls[0]}" sed ''"$random"'q;d')

    # Download image from the server

    curl -s -L "$randomurl" -o "${wallpaperpath[0]}"

    # Set dummy image as wallpaper so Finder will change the wallpaper to the pic that we want

    # No langer needed as we choose a new name every time

    # echo "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=" | base64 -D -o "/tmp/dummyimg.jpg"

    # osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/private/tmp/dummyimg.jpg"'

    # Set image as wallpaper

    if [ "${wallpaperpath[0]}" == "/tmp/wallpaper.jpg" ]; then
 
    osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper.jpg"'

    else

    osascript -e 'tell application "Finder" to set desktop picture to POSIX file "/tmp/wallpaper1.jpg"'

    fi

    # Cleanup

    rm /tmp/imageurls.txt

    echo "Random Url: $random"

    exit
fi
