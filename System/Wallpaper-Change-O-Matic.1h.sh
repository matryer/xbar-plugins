#!/usr/bin/env bash

#<bitbar.title>Reddit Wallpaper Change-O-Matic</bitbar.title>
#<bitbar.version>v1.0</bitbar.version>
#<bitbar.author>Manu Wallner</bitbar.author>
#<bitbar.author.github>milch</bitbar.author.github>
#<bitbar.desc>Choose a random image from a list of subreddits and set it as a desktop wallpaper.</bitbar.desc>
#<bitbar.image>tbd</bitbar.image>
#<bitbar.dependencies>Xcode,swift</bitbar.dependencies>

# Finds the directory the script is stored in
# Taken from http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Call make, silence regular output
sh -c "cd $DIR/Change-O-Matic && make" 1>/dev/null
$DIR/Change-O-Matic/rwcom
