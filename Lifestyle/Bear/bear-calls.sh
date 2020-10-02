#!/bin/bash
# <bitbar.author>mreider</bitbar.author>
# <bitbar.author.github>mreider</bitbar.author.github>
# <bitbar.title>Handy bear calls</bitbar.title>
# <bitbar.version>1.0</bitbar.version>

Replace these with your home page and bookmarks page

HOMEPAGE='bear://x-callback-url/open-note?id=3B82C004-4B7D-4B66-A492-6D0F3EEDA40A-4300-00001FDE04293478'
BOOKMARKS='bear://x-callback-url/open-note?id=68496A87-42BE-479A-AE9C-3117CB1CB9FB-31243-00045FA30CF5DC43'

rawurlencode() {
  local string="${1}"
  local strlen=${#string}
  local encoded=""
  local pos c o

  for (( pos=0 ; pos<strlen ; pos++ )); do
     c=${string:$pos:1}
     case "$c" in
        [-_.~a-zA-Z0-9] ) o="${c}" ;;
        * )               printf -v o '%%%02x' "'$c"
     esac
     encoded+="${o}"
  done
  echo "${encoded}"    # You can either set a return variable (FASTER) 
  REPLY="${encoded}"   #+or echo the result (EASIER)... or both... :p
}


echo "🐻"
echo "---"
HOMEPAGE_LINK='[Home Page]('$HOMEPAGE')'
HOMEPAGE_ENCODED=$( rawurlencode "$HOMEPAGE_LINK" )
TITLE='Notes For '$(date +"%a %b %d %Y")
TITLE_ENCODED=$( rawurlencode "$TITLE" )
DATE_WITH_HOME_LINK='bear://x-callback-url/create?title='$TITLE_ENCODED'&text='$HOMEPAGE_ENCODED
echo "Open home page | href=$HOMEPAGE"
echo "Open bookmarks | href=$BOOKMARKS"
echo "New note with date & home link | href=$DATE_WITH_HOME_LINK"


