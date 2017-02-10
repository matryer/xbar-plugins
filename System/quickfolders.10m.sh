#!/bin/bash
# <bitbar.title>QuickFolders</bitbar.title>
# <bitbar.version>v1.0</bitbar.version>
# <bitbar.author>Alex</bitbar.author>
# <bitbar.author.github>alexrockt</bitbar.author.github>
# <bitbar.desc>Quick access to folders, that have files with certain suffix in it. For more details look at https://blog.aruehe.io/quickfolders-a-bitbar-plugin/</bitbar.desc>
# <bitbar.image>https://raw.githubusercontent.com/alexrockt/misc/master/quickfolders-screenshot.png</bitbar.image>
# <bitbar.dependencies>bash</bitbar.dependencies>
# <bitbar.abouturl>https://blog.aruehe.io/quickfolders-a-bitbar-plugin/</bitbar.abouturl>

PATH_TO_FOLDER=/path/to/a/folder

#FILETYPES TO NOTE ( e.g. pdf|doc) this will be put into grep for filtering
FILETYPES="pdf|doc"
#Regular expression, to filtering during the find operation. E.g. to exclude . files
REX=".*/\..*"

#This variable stores the result
ALLFOLDERS=$(find "${PATH_TO_FOLDER}" \( ! -regex "$REX" \) -type f | grep -E "($FILETYPES)" | sed -E 's|/[^/]+$||' | uniq | sort)

IFS='
'

echo "QuickFolders | color=green size=10"
echo "---"

LASTFOLDER=""
LASTSUBFOLDER=""

for l in $ALLFOLDERS
do
    FOLDER=$(echo "$l" | awk -F/ '{print $(NF-1)}')
    SUBFOLDER=$(echo "$l" | awk -F/ '{print $(NF)}')
    # same folder as before, check if same subfolder
    if [ "$LASTFOLDER" = "$FOLDER" ]; then
        # new folder - print
        if [ "$LASTSUBFOLDER" != "$SUBFOLDER" ]; then
            printf "┗━━━%s | terminal=false refresh=true bash=/usr/bin/open param1='%s' size=10 color=green\n" "$SUBFOLDER" "$l"
            LASTSUBFOLDER=$SUBFOLDER
        fi
    else
        printf "%s | size=12 color=white\n" "$FOLDER"
        printf "┗━━━%s | terminal=false refresh=true bash=/usr/bin/open param1='%s' size=10 color=green\n" "$SUBFOLDER" "$l"
        LASTFOLDER=$FOLDER
        LASTSUBFOLDER=$SUBFOLDER
    fi
done