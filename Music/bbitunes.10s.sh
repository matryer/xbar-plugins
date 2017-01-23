#!/bin/bash
#
# Lightweight iTunes status plugin for BitBar
#
# Author: Padraic Renaghan
#
# based on iTunes script:
# https://github.com/matryer/bitbar-plugins/blob/master/Music/itunes.10s.sh

# metadata
# <bitbar.title>iTunes Lite</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Padraic Renaghan</bitbar.author>
# <bitbar.author.github>prenagha</bitbar.author.github>
# <bitbar.desc>Display current track info from iTunes</bitbar.desc>
# <bitbar.image>https://github.com/prenagha/bitbar-itunes/raw/master/bbitunes.png</bitbar.image>
# <bitbar.dependencies>iTunes Lite applescript</bitbar.dependencies>
# <bitbar.abouturl>https://github.com/prenagha/bitbar-itunes</bitbar.abouturl>

# set this to the directory of the cloned/downloaded github repo
BBITUNES_DIR="$HOME/Dev/bitbar-itunes"

# this will point to the .applescript source file in the repo
APPLESCRIPT="${BBITUNES_DIR}/bbitunes.applescript"

# this will be the compiled version of the source file
COMPILED="${BBITUNES_DIR}/bbitunes.scpt"

# link to installation instructions
SETUP="View Setup Instructions | bash=/usr/bin/open param1=https://github.com/prenagha/bitbar-itunes terminal=false"

# check that we can find the repo dir
if [ ! -d "${BBITUNES_DIR}" ]
then
  echo -e "Dir not found\n---\n${BBITUNES_DIR} not found\n${SETUP}"
  exit 1
fi

# check that the source file is in the repo dir
if [ ! -f "${APPLESCRIPT}" ]
then
  echo -e "Applescript not found\n---\n${APPLESCRIPT} not found\n${SETUP}"
  exit 2
fi

# if the compiled file is missing, or the source is newer than
# the compiled file, then recompile
if [ ! -f "${COMPILED}" ] || [ "${APPLESCRIPT}" -nt "${COMPILED}" ]
then
  /usr/bin/osacompile -o "${COMPILED}" "${APPLESCRIPT}"
fi

# run the compiled applescript which does all the work to talk to iTunes
/usr/bin/osascript "${COMPILED}" "${BASH_SOURCE[0]}" "$1"
